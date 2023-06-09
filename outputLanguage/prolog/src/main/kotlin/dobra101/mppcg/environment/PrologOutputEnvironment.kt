package dobra101.mppcg.environment

import dobra101.mppcg.IndividualInfo
import dobra101.mppcg.RenderResult
import dobra101.mppcg.environment.PrologOutputEnvironment.Companion.BEFORE
import dobra101.mppcg.environment.PrologOutputEnvironment.Companion.EXPRESSION_SEPARATOR
import dobra101.mppcg.environment.PrologOutputEnvironment.Companion.RESULT_EXPR
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import kotlin.math.max

/**
 * This is the [OutputLanguageEnvironment] for Prolog.
 *
 * Here, the ```renderSelf```-method is implemented for each node.
 */
class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    var evaluatedExpressions = EvaluatedExpressions()

    companion object {
        const val EXPRESSION_SEPARATOR = ",\n"
        const val BEFORE = "before"
        const val RESULT_EXPR = "resultExpr"
    }

    private var codeRepresentation: Program? = null
    private var exprCount = 0
    private var stateCount = 0
    private var operationParameters: List<IdentifierExpression> = emptyList() // HINT: only for B
    private var comprehensionSetIdentifier: List<IdentifierExpression> = emptyList() // HINT: only for B
    private var lambdaExpressionIdentifier: List<IdentifierExpression> = emptyList() // HINT: only for B
    private var temporaryVariables: Set<IdentifierExpression> = hashSetOf() // HINT: only for B
    private var concreteConstants: List<Expression> = emptyList() // HINT: only for B
    private var constants: List<Expression> = emptyList() // HINT: only for B
    private var concreteVariables: List<Expression> = emptyList() // HINT: only for B
    private var variables: ClassVariables = ClassVariables() // HINT: only for B
    private val whileDefinitions: MutableList<String> = mutableListOf() // HINT: only for B
    private var whileCount = 0 // HINT: only for B
    private var quantifierCount = 0 // HINT: only for B
    private val quantifierDefinitions: MutableMap<QuantifierPredicate, String> = hashMapOf() // HINT: only for B
    private var currentOperation: String = "" // HINT: only for B
    private var declarationStep: Boolean = true // HINT: only for B
    private var ignoreOutput: Boolean = false // HINT: only for B
    private var returnValues: List<Expression> = listOf() // HINT: only for B
    private var ctrlStructIdentifier: List<IdentifierExpression> = listOf() // HINT: only for B
    private var concreteIdentifierExpressionDeclared: MutableMap<String, Boolean> = mutableMapOf() // HINT: only for B

    private fun expr(name: Any): String = "Expr_$name"

    /**
     * Returns the evaluated expression or evaluates the node.
     */
    private fun loadOrEvaluate(node: MPPCGNode, evaluation: EvaluatedExpressions.() -> RenderResult): RenderResult {
        if (evaluatedExpressions.containsKey(node)) return RenderResult(evaluatedExpressions[node]!!)
        return evaluation(evaluatedExpressions)
    }

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousCollectionNode.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedExpressionList.of(elements)
            val map = mapOf(
                "elements" to expanded.expressions
            )

            if (expanded.before.isBlank()) {
                return@loadOrEvaluate RenderResult(renderTemplate(map))
            }

            val rendered = renderTemplate(map)
            val before = expanded.before.removeSuffix(EXPRESSION_SEPARATOR)

            add(node, rendered)
            return@loadOrEvaluate RenderResult(
                before,
                mapOf(RESULT_EXPR to IndividualInfo(rendered))
            )
        }
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedBinary.of(left, right)
            val isInfiniteSet =
                operator == BinaryExpressionOperator.POW || left.type is TypeSet || right.type is TypeSet

            val map = mapOf(
                "lhs" to expanded.lhs,
                "rhs" to expanded.rhs,
                "operator" to operator2String(operator, isInfiniteSet),
                "exprCount" to exprCount,
                "math" to isMathOperator(operator),
                "infiniteSet" to isInfiniteSet
            )

            // TODO: add to evaluated?
            val rendered = renderTemplate(map)
            return@loadOrEvaluate RenderResult("${expanded.before}$rendered", exprToInfo(node))
        }
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val map = mapOf(
                "name" to name,
                "elements" to elements.render(),
                "isParameter" to isParameter,
                "exprCount" to exprCount
            )

            add(node, expr(exprCount))

            if (isParameter) {
                val info = mapOf(RESULT_EXPR to IndividualInfo(expr(exprCount)))
                // expr is now assigned -> increase
                exprCount++
                return@loadOrEvaluate RenderResult(renderTemplate(map), info)
            }
            return@loadOrEvaluate RenderResult(renderTemplate(map))
        }
    }

    // HINT: same as SetEntry
    override fun EnumEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        fun renderNamed(exprName: String, state: Int = stateCount): RenderResult {
            val map = mapOf(
                "name" to name,
                "stateCount" to state,
                "exprCount" to exprName
            )
            val rendered = renderTemplate(map)
            if (optimize) evaluatedExpressions[this] = expr(exprName)
            return RenderResult(rendered, mapOf(RESULT_EXPR to IndividualInfo(expr(exprName))))
        }

        fun isEnumValue(): Boolean {
            return (codeRepresentation as Machine).sets.find { it.elements.find { e -> e.name == name } != null } != null
        }

        return loadOrEvaluate(this) {
            if (operationParameters.contains(node)
                || comprehensionSetIdentifier.contains(node)
                || lambdaExpressionIdentifier.contains(node)
            ) {
                add(node, expr(name))
                return@loadOrEvaluate RenderResult(expr(name))
            }

            if (node.type is TypeEnumValue || isEnumValue()) {
                add(node, "'$name'")
                return@loadOrEvaluate RenderResult("'$name'")
            }

            // TODO: not hardcoded
            if (ctrlStructIdentifier.contains(node)) {
                add(node, expr("q_$name"))
                return@loadOrEvaluate RenderResult(expr("q_$name"))
            }

            // TODO: not hardcoded
            if (temporaryVariables.contains(node)) {
                add(node, "tmp_$name")
                return@loadOrEvaluate renderNamed("tmp_$name", 0)
            }

            // TODO: not hardcoded
            val rendered =
                if (isConstant(node as IdentifierExpression)) {
                    // use constant prefix
                    val cc = ConcreteIdentifierExpression(name, node, type)
                    "${cc.name}(${expr(exprCount)})"
                } else {
                    val map = mapOf(
                        "name" to name,
                        "stateCount" to stateCount,
                        "exprCount" to exprCount
                    )
                    renderTemplate(map)
                }
            return@loadOrEvaluate RenderResult(rendered, exprToInfo(node))
        }
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs
        )
        return RenderResult(renderTemplate(map), info = mapOf(BEFORE to IndividualInfo(expanded.before)))
    }

    override fun SetCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: same as EnumEntry
    override fun SetEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(renderTemplate(map))
    }

    // HINT: Same for Java and Prolog
    override fun ValueExpression.renderSelf(): RenderResult {
        val map = mapOf("value" to value)

        return RenderResult(renderTemplate(map))
    }


    /* ---------- PREDICATES ---------- */
    // HINT: SAME AS LOGIC PREDICATE (except optimization)
    override fun BinaryPredicate.renderSelf(): RenderResult {
        fun optimizable(): Boolean {
            if (operator != BinaryPredicateOperator.EQUAL && operator != BinaryPredicateOperator.NOT_EQUAL) return false
            if (right !is IdentifierExpression && right !is CollectionEntry && right !is ValueExpression) return false
            if (left !is IdentifierExpression) return false
            if (ctrlStructIdentifier.contains(left)) return false
            if (temporaryVariables.contains(left)) return false
            return !operationParameters.contains(left)
        }

        fun ValueExpression.rendered(): String {
            return when (valueType) {
                MPPCG_Boolean -> render().rendered
                is TypeNumber -> render().rendered
                else -> ""
            }
        }

        // optimize
        //
        // Example:
        // "get(State_0, 'x', Expr_0), Expr_0 = 2" will be rewritten to "get(State_0, 'x', 2)"
        if (optimizable()) {
            var before = ""
            val rhs = when (right) {
                is IdentifierExpression -> {
                    if (isConstant((right as IdentifierExpression)) || isVariable((right as IdentifierExpression))) {
                        evaluatedExpressions[right] ?: run {
                            before = right.render().rendered
                            before += EXPRESSION_SEPARATOR
                            evaluatedExpressions[right]
                        }
                    } else {
                        "'${(right as IdentifierExpression).name}'"
                    }
                    evaluatedExpressions[right] ?: "'${(right as IdentifierExpression).name}'"
                }

                is SetEntry -> "'${(right as SetEntry).name}'"
                is CollectionEntry -> "'${(right as CollectionEntry).name}'"
                is ValueExpression -> (right as ValueExpression).rendered()
                else -> "" // when is exhaustive
            }

            val lhs = when {
                comprehensionSetIdentifier.contains(left) -> left.render().rendered
                ctrlStructIdentifier.contains(left) -> left.render().rendered
                else -> (left as IdentifierExpression).name
            }

            val map = mapOf(
                "lhs" to lhs,
                "rhs" to rhs,
                "stateCount" to stateCount,
                "negate" to (operator == BinaryPredicateOperator.NOT_EQUAL),
                "useGet" to (!ctrlStructIdentifier.contains(left) && !comprehensionSetIdentifier.contains(left))
            )
            val rendered = renderTemplate("optimizedBinaryPredicateEqual", map)
            return RenderResult("$before$rendered")
        }


        val differentBranches = operator == BinaryPredicateOperator.EQUAL
                && (
                (left as? UnaryExpression)?.operator == UnaryExpressionOperator.CONVERT_BOOLEAN
                        || (right as? UnaryExpression)?.operator == UnaryExpressionOperator.CONVERT_BOOLEAN
                )
        val expanded =
            ExpandedBinary.of(left, right, differentBranches = differentBranches, this@PrologOutputEnvironment)

        val prefixOperators = listOf(
            BinaryPredicateOperator.MEMBER,
            BinaryPredicateOperator.NOT_MEMBER,
            BinaryPredicateOperator.SUBSET,
            BinaryPredicateOperator.STRICT_SUBSET,
            BinaryPredicateOperator.EQUAL,
            BinaryPredicateOperator.NOT_EQUAL
        )

        val infiniteSet = left.type is TypeSet || right.type is TypeSet
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator, infiniteSet),
            "prefixOperator" to (prefixOperators.contains(operator) || infiniteSet)
        )
        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered")
    }

    // HINT: SAME AS BINARY PREDICATE (except optimization)
    override fun BinaryLogicPredicate.renderSelf(): RenderResult {
        fun inline(lineBreaks: Int): Boolean {
            if (lineBreaks != 0) return false
            if (operator == LogicPredicateOperator.AND) return false
            if (left !is BinaryPredicate) return false
            return right is BinaryPredicate
        }

        val expanded = ExpandedBinary.of(left, right, true, this@PrologOutputEnvironment)
        return when (operator) {
            LogicPredicateOperator.EQUIVALENCE -> {
                val map = mapOf(
                    "lhs" to expanded.lhs,
                    "rhs" to expanded.rhs
                )
                RenderResult(renderTemplate("equivalence", map))
            }

            LogicPredicateOperator.IMPLIES -> {
                val lineBreaksTotal = expanded.lhs.count { it == '\n' } + expanded.rhs.count { it == '\n' }

                val map = mapOf(
                    "lhs" to expanded.lhs,
                    "rhs" to expanded.rhs,
                    "inline" to inline(lineBreaksTotal)
                )
                RenderResult(renderTemplate("implication", map))
            }

            else -> {
                val lineBreaksTotal = expanded.lhs.count { it == '\n' } + expanded.rhs.count { it == '\n' }
                val map: MutableMap<String, Any> = mutableMapOf(
                    "lhs" to expanded.lhs,
                    "rhs" to expanded.rhs,
                    "operator" to operator2String(operator),
                    "addParentheses" to (operator == LogicPredicateOperator.OR),
                    "inline" to inline(lineBreaksTotal)
                )

                RenderResult("${expanded.before}${renderTemplate(map)}")
            }
        }
    }

    override fun UnaryLogicPredicate.renderSelf(): RenderResult {
        // evaluated needs reset because of scoping
        var map: Map<String, Any> = mapOf()
        evaluatedExpressions.withReset {
            map = mapOf(
                "predicate" to predicate.render(),
                "operator" to operator2String(operator)
            )
        }
        return RenderResult(renderTemplate(map))
    }

    override fun ValuePredicate.renderSelf(): RenderResult {
        val map = mapOf("value" to value)
        return RenderResult(renderTemplate(map))
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        if (ignoreOutput && returnValues.contains(left)) {
            return RenderResult("true")
        }

        val identifier = when (left) {
            is IdentifierExpression -> "'${(left as IdentifierExpression).name}'"

            is CallFunctionExpression -> {
                val map = mapOf(
                    "expression" to ((left as CallFunctionExpression).expression as IdentifierExpression).name,
                    "parameters" to (left as CallFunctionExpression).parameters.render(),
                )
                renderTemplate("callFunctionAccess", map)
            }

            else -> left.render().rendered
        }

        val expandedRhs = ExpandedExpression.of(right)
        val needTempVar = (temporaryVariables.contains(left) && !evaluatedExpressions.contains(left)) && false
        val map = mapOf(
            "identifier" to identifier,
            "rhs" to expandedRhs.expression,
            "stateCount" to stateCount,
            "resultStateCount" to ++stateCount,
            "needTmpVar" to needTempVar,
            "tmpVar" to "tmp_${identifier.removeSurrounding("'")}"
        )
        val rendered = renderTemplate(map)

        if (optimize) {
            if (needTempVar) {
                evaluatedExpressions[left] = expr("tmp_${identifier.removeSurrounding("'")}")
            } else if (!temporaryVariables.contains(left)) {
                evaluatedExpressions[left] = expandedRhs.expression
            }
        }
        return RenderResult("${expandedRhs.before}$rendered")
    }

    override fun DeclarationSubstitution.renderSelf(): RenderResult {
        return assignment.render()
    }

    override fun ElseIfSubstitution.renderSelf(): RenderResult {
        // TODO: consider state and expr count?
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IfSubstitution.renderSelf(): RenderResult {
        fun addStateAndExprCountFixingElseBranch(
            stateCountBefore: Int,
            exprCountBefore: Int,
            map: MutableMap<String, Any>,
            thenRendered: RenderResult
        ) {
            val elseBranch = StringBuilder()
            if (stateCountBefore < stateCount) {
                val countMap = mapOf("newCount" to stateCount, "oldCount" to stateCountBefore)
                elseBranch.append(renderTemplate("updateStateCount", countMap))
            }

            if (exprCountBefore < exprCount) {
                val countMap = mapOf("newCount" to exprCount, "oldCount" to exprCountBefore)
                if (elseBranch.isNotBlank()) {
                    elseBranch.append(EXPRESSION_SEPARATOR)
                }
                elseBranch.append(renderTemplate("updateExprCount", countMap))
            }
            map["then"] = thenRendered
            if (elseBranch.isNotBlank()) {
                map["elseSubstitution"] = elseBranch.toString()
            }
        }

        fun fixStateAndExprCountInBranches(
            thenRendered: RenderResult,
            elseRendered: RenderResult,
            stateCountAfterThen: Int,
            exprCountAfterThen: Int,
            map: MutableMap<String, Any>
        ) {
            // set the stateCount and exprCount for the main branch
            val thenStringBuilder = StringBuilder(thenRendered.rendered)
            val elseStringBuilder = StringBuilder(elseRendered.rendered)

            // stateCount
            if (stateCountAfterThen < stateCount) {
                val countMap = mapOf("newCount" to stateCount, "oldCount" to stateCountAfterThen)
                thenStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateStateCount", countMap))
            } else if (stateCountAfterThen > stateCount) {
                val countMap = mapOf("newCount" to stateCountAfterThen, "oldCount" to stateCount)
                elseStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateStateCount", countMap))
            }

            // exprCount
            if (exprCountAfterThen < exprCount) {
                val countMap = mapOf("newCount" to exprCount - 1, "oldCount" to exprCountAfterThen - 1)
                thenStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateExprCount", countMap))
            } else if (exprCountAfterThen > exprCount) {
                val countMap = mapOf("newCount" to exprCountAfterThen - 1, "oldCount" to exprCount - 1)
                elseStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateExprCount", countMap))
            }

            map["then"] = RenderResult(thenStringBuilder.toString(), thenRendered.info)
            map["elseSubstitution"] = RenderResult(elseStringBuilder.toString(), elseRendered.info)
        }

        val map = mutableMapOf(
            "condition" to condition.render(),
            "elseIf" to elseIf.render()
        )

        val stateCountBefore = stateCount
        val exprCountBefore = exprCount

        val thenRendered = evaluatedExpressions.returnWithReset(true) {
            then.render()
        }


        if (elseSubstitution == null) {
            addStateAndExprCountFixingElseBranch(stateCountBefore, exprCountBefore, map, thenRendered)
        } else {
            val stateCountAfterThen = stateCount
            val exprCountAfterThen = exprCount
            stateCount = stateCountBefore
            exprCount = exprCountBefore

            val elseRendered = evaluatedExpressions.returnWithReset(true) {
                elseSubstitution!!.render()
            }

            fixStateAndExprCountInBranches(
                thenRendered = thenRendered,
                elseRendered = elseRendered,
                stateCountAfterThen = stateCountAfterThen,
                exprCountAfterThen = exprCountAfterThen,
                map = map
            )

            stateCount = max(stateCountAfterThen, stateCount)
            exprCount = max(exprCountAfterThen, exprCount)
        }

        return RenderResult(renderTemplate(map))
    }

    override fun SequenceSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun WhileSubstitution.renderSelf(): RenderResult {
        fun getDefinitionMap(): Map<String, Any> {
            return mapOf(
                "name" to currentOperation,
                "count" to whileCount,
                "condition" to evaluatedExpressions.returnWithReset(true) { condition.render() },
                "body" to evaluatedExpressions.returnWithReset(true) { body.render() },
                "lastState" to stateCount
            )
        }

        val stateCountBefore = stateCount
        val exprCountBefore = exprCount
        stateCount = 0
        exprCount = 0

        val whileDefinitionMap = getDefinitionMap()

        stateCount = stateCountBefore
        exprCount = exprCountBefore
        val map = mapOf(
            "name" to currentOperation,
            "count" to whileCount,
            "stateCount" to stateCount++,
            "resultStateCount" to stateCount
        )
        val renderedDefinition = renderTemplate("whileDefinition", whileDefinitionMap)
        if (!whileDefinitions.contains(renderedDefinition)) whileDefinitions.add(renderedDefinition)
        whileCount++

        return RenderResult(renderTemplate(map))
    }

    /* ---------- CLASS BLOCK ---------- */
    override fun ClassVariables.renderSelf(): RenderResult {
        // not needed in prolog
        return RenderResult()
    }

    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "type" to functionType,
            "mapType" to mapType
        )
        return RenderResult(renderTemplate(map), info = mapOf(BEFORE to IndividualInfo(expanded.before)))
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedBinary.of(left, right)

            val map = mapOf(
                "lhs" to expanded.lhs,
                "rhs" to expanded.rhs,
                "operator" to operator2String(operator),
                "exprCount" to exprCount
            )
            val rendered = renderTemplate(map)

            return@loadOrEvaluate RenderResult("${expanded.before}$rendered", exprToInfo(node))
        }
    }

    // TODO: reuse BinaryExpression?
    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedBinary.of(left, right)

            val map = mapOf(
                "lhs" to expanded.lhs,
                "rhs" to expanded.rhs,
                "operator" to operator2String(operator),
                "exprCount" to exprCount
            )
            val rendered = renderTemplate(map)

            return@loadOrEvaluate RenderResult("${expanded.before}$rendered", exprToInfo(node))
        }
    }

    override fun BinarySequenceExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedBinary.of(left, right)

            // TODO: refactor append/prepend
            val map = mapOf(
                "lhs" to expanded.lhs,
                "rhs" to if (operator == BinarySequenceExpressionOperator.APPEND) "[${expanded.rhs}]" else expanded.rhs,
                "operator" to operator2String(operator),
                "exprCount" to exprCount
            )
            val rendered = renderTemplate(map)

            return@loadOrEvaluate RenderResult("${expanded.before}$rendered", exprToInfo(node))
        }
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val before1 = ExpandedExpression.of(expression)
            val before2 = ExpandedExpressionList.of(parameters)

            val map = mapOf(
                "expression" to before1.expression,
                "parameters" to before2.expressions,
                "exprCount" to exprCount,
            )

            val before = "${before1.before}${before2.before}"
            val rendered = renderTemplate(map)

            return@loadOrEvaluate RenderResult("$before$rendered", exprToInfo(node))
        }
    }

    override fun ComprehensionSet.renderSelf(): RenderResult {
        fun nestedIdentifier(ids: List<Expression>): String {
            if (ids.size == 1) {
                return identifiers[0].render().rendered

            }
            if (ids.size == 2) {
                val map = mapOf(
                    "first" to ids[0].render(),
                    "second" to ids[1].render(),
                )

                return renderTemplate("comprehensionSetIdentifier", map)
            }

            val map = mapOf(
                "first" to ids[0].render(),
                "second" to nestedIdentifier(ids.subList(1, ids.size)),
            )

            return renderTemplate("comprehensionSetIdentifier", map)
        }

        return evaluatedExpressions.returnWithReset {
            comprehensionSetIdentifier += identifiers.filterIsInstance<IdentifierExpression>()
            val map = evaluatedExpressions.returnWithReset {
                mapOf(
                    "identifiers" to nestedIdentifier(identifiers),
                    "predicates" to predicates.render(),
                    "exprCount" to exprCount
                )
            }
            comprehensionSetIdentifier -= identifiers.filterIsInstance<IdentifierExpression>().toSet()
            return@returnWithReset RenderResult(renderTemplate(map), exprToInfo(node))
        }
    }

    override fun ConcreteIdentifierExpression.renderSelf(): RenderResult {
        if (declarationStep) {
            stateCount = 0

            val renderedValue = value.render()
            val before = if (renderedValue.containsKey(BEFORE)) renderedValue[BEFORE].info else ""
            val map = mapOf(
                "name" to name,
                "before" to before.removeSuffix(EXPRESSION_SEPARATOR),
                "value" to renderedValue.rendered,
                "interval" to (value is IntervalExpression),
                "inline" to (!renderedValue.rendered.contains(EXPRESSION_SEPARATOR) && value !is ComprehensionSet && value !is LambdaExpression && before.isBlank()), // TODO: type checks needed?
                "exprCount" to exprCount - 1 // last assigned expression
            )
            val rendered = RenderResult(renderTemplate("concreteIdentifierDeclaration", map))
            concreteIdentifierExpressionDeclared[rendered.rendered] = !((map["inline"] as Boolean) || (map["interval"] as Boolean))
            return rendered
        }

        return loadOrEvaluate(this) {
            val map = mapOf(
                "name" to name,
                "exprCount" to exprCount
            )

            add(node, expr(exprCount))
            return@loadOrEvaluate RenderResult(renderTemplate(map), exprToInfo(node))
        }
    }

    override fun Couple.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(from, to)
        val map = mapOf(
            "from" to expanded.lhs,
            "to" to expanded.rhs
        )

        return RenderResult(renderTemplate(map), info = mapOf(BEFORE to IndividualInfo(expanded.before)))
    }

    override fun GeneralSumOrProductExpression.renderSelf(): RenderResult {
        ctrlStructIdentifier += identifiers

        val map = evaluatedExpressions.returnWithReset {
            mapOf(
                "identifiers" to identifiers.render(),
                "predicate" to predicate.render(),
                "expression" to if (expression !is IdentifierExpression) expression.render() else null,
                "isSum" to (operation == SumOrProductOperation.SUM),
                "exprCount" to exprCount
            )
        }

        ctrlStructIdentifier -= identifiers

        return RenderResult(renderTemplate(map), exprToInfo(this))
    }

    override fun InfiniteSet.renderSelf(): RenderResult {
        return RenderResult(renderTemplate(mapOf("type" to type2String(type!!))))
    }

    override fun LambdaExpression.renderSelf(): RenderResult {
        lambdaExpressionIdentifier = identifiers.filterIsInstance<IdentifierExpression>()
        val map = mapOf(
            "identifier" to identifiers.render(),
            "predicate" to predicate.render(),
            "expression" to expression.render(),
            "value" to expr(exprCount - 1),
            "exprCount" to exprCount
        )
        lambdaExpressionIdentifier = emptyList()

        return RenderResult(renderTemplate(map), exprToInfo(this))
    }

    override fun Sequence.renderSelf(): RenderResult {
        val map = mapOf(
            "elements" to elements.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnarySequenceExpression.renderSelf(): RenderResult {
        val expanded = ExpandedExpression.of(sequence)
        val map = mapOf(
            "sequence" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        return RenderResult("${expanded.before}${renderTemplate(map)}", exprToInfo(this))
    }

    override fun UnaryCollectionExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedExpression.of(collection)

            val map = mutableMapOf(
                "collection" to expanded.expression,
                "operator" to operator2String(operator),
                "exprCount" to exprCount
            )

            return@loadOrEvaluate RenderResult("${expanded.before}${renderTemplate(map)}", exprToInfo(node))
        }
    }

    override fun UnaryExpression.renderSelf(): RenderResult {
        // TODO: optimize minus
        return loadOrEvaluate(this) {
            var before = ""
            var rendered = ""
            evaluatedExpressions.withReset(operator == UnaryExpressionOperator.CONVERT_BOOLEAN) {
                val renderedValue = if (evaluatedExpressions.contains(value)) {
                    null
                } else if (value is Expression && operator != UnaryExpressionOperator.MINUS) {
                    val expanded = ExpandedExpression.of(value as Expression)
                    before = expanded.before
                    expanded.expression
                } else {
                    value.render().rendered
                }
                val resultAt =
                    if (evaluatedExpressions.contains(value)) evaluatedExpressions[value] else expr(exprCount - 1)

                val map = mapOf(
                    "value" to renderedValue,
                    "operator" to operator2String(operator),
                    "convertBoolean" to (operator == UnaryExpressionOperator.CONVERT_BOOLEAN),
                    "isMinus" to (operator == UnaryExpressionOperator.MINUS),
                    "isMinusInline" to (value is ValueExpression || value is ConcreteIdentifierExpression),
                    "resultAt" to resultAt,
                    "exprCount" to exprCount
                )
                rendered = renderTemplate(map)
            }

            return@loadOrEvaluate RenderResult("$before$rendered", exprToInfo(node))
        }
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        return loadOrEvaluate(this) {
            val expanded = ExpandedExpression.of(expression)

            val map = mapOf(
                "expression" to expanded.expression,
                "operator" to operator2String(operator),
                "exprCount" to exprCount
            )

            val rendered = renderTemplate(map)

            return@loadOrEvaluate RenderResult("${expanded.before}$rendered", exprToInfo(node))
        }
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        val checkInvariants = List(predicates.size) { idx ->
            evaluatedExpressions.clear()
            exprCount = 0
            stateCount = 0
            val map = mapOf(
                "body" to predicates[idx].render(),
                "idx" to idx
            )
            renderTemplate(map)
        }

        val renderedCheckInvariants = renderTemplate("invariants", mapOf("list" to checkInvariants))

        val map = mapOf(
            "invariants" to renderedCheckInvariants,
            "indices" to predicates.indices.map { it }
        )

        return RenderResult(renderTemplate("props", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        fun getDefinitionMap(thisQuantifierCount: Int): Map<String, Any?> {
            return evaluatedExpressions.returnWithReset(true) {
                mapOf(
                    "name" to currentOperation,
                    "count" to thisQuantifierCount,
                    "predicate" to predicate.render(),
                    "quantification" to quantification?.render(),
                    "parameters" to (operationParameters + temporaryVariables + ctrlStructIdentifier - identifier.toSet()).map { it.render().rendered }
                )
            }
        }

        val thisQuantifierCount = quantifierCount
        quantifierCount++
        val stateCountBefore = stateCount
        val exprCountBefore = exprCount
        stateCount = 0
        exprCount = 0

        ctrlStructIdentifier = ctrlStructIdentifier + identifier
        val definitionMap = getDefinitionMap(thisQuantifierCount)

        stateCount = stateCountBefore
        exprCount = exprCountBefore

        val map = evaluatedExpressions.returnWithReset {
            mapOf(
                "name" to currentOperation,
                "count" to thisQuantifierCount,
                "parameters" to definitionMap["parameters"],
                "stateCount" to stateCount,
                "predicate" to predicate.render(),
                "quantification" to quantification?.render(),
                "universalQuantifier" to (type == QuantifierType.FORALL)
            )
        }
        ctrlStructIdentifier = ctrlStructIdentifier - identifier.toSet()

        val renderedDefinition = renderTemplate("quantifierDefinition", definitionMap)
        if (!quantifierDefinitions.contains(this)) quantifierDefinitions[this] = renderedDefinition

        return RenderResult(renderTemplate(map))
    }

    override fun Initialization.renderSelf(): RenderResult {
        evaluatedExpressions.clear()

        val map = mapOf(
            "body" to substitutions.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun ParallelSubstitution.renderSelf(): RenderResult {
        temporaryVariables = temporaryVariables + needTempVar

        // remove temporary variables from previous evaluated to use temp vars
        if (optimize) temporaryVariables.forEach { evaluatedExpressions.remove(it) }

        val neededTempVars = substitutions
            .filterIsInstance<AssignSubstitution>()
            .filter { it.right is IdentifierExpression && temporaryVariables.contains(it.right) }
            .map { it.right as IdentifierExpression }

        // TODO: let optimizer do the work
        // load temporary variables if they are needed
        // optimized: they are not needed if e.g. 'a = a + 1 || b = a + 1' and a + 1 has been evaluated before
        val tempVars = needTempVar.mapNotNull {
            if (optimize && !neededTempVars.contains(it)) {
                null
            } else {
                val map = mapOf(
                    "name" to it.name,
                    "stateCount" to stateCount,
                    "exprCount" to "tmp_${it.name}"
                )
                renderTemplate(it.templateName, map)
            }
        }

        val map = mapOf(
            "tempVars" to tempVars,
            "substitutions" to substitutions.render()
        )

        temporaryVariables = temporaryVariables - needTempVar

        return RenderResult(renderTemplate(map))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Precondition.renderSelf(): RenderResult {
        evaluatedExpressions.clear()

        val map = mapOf(
            "predicate" to predicate.render().rendered,
            "substitution" to substitution?.render()?.rendered
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Select.renderSelf(): RenderResult {
        // TODO: add when, else
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then?.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Machine.renderSelf(): RenderResult {
        codeRepresentation = this

        declarationStep = true
        this@PrologOutputEnvironment.concreteConstants = concreteConstants
        this@PrologOutputEnvironment.constants = constants
        evaluatedExpressions.clear()
        stateCount = 0
        exprCount = 0
        temporaryVariables = hashSetOf()
        operationParameters = parameters.map { it as IdentifierExpression }
        this@PrologOutputEnvironment.concreteVariables = concreteVariables
        this@PrologOutputEnvironment.variables = variables

        concreteConstants.render()

        declarationStep = false

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteIdentifierExpressionDeclared.toList().sortedBy { it.second }.map { it.first }.toList(),
//            "properties" to properties?.render(), // TODO: use properties
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization.render(),
            "invariant" to invariant.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "whileDefinitions" to whileDefinitions,
            "quantifierDefinitions" to quantifierDefinitions.values
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Operation.renderSelf(): RenderResult {
        fun resetEnvironment() {
            evaluatedExpressions.clear()
            stateCount = 0
            exprCount = 0
            temporaryVariables = hashSetOf()
            operationParameters = parameters.map { it as IdentifierExpression }
            currentOperation = name
            whileCount = 0
            quantifierCount = 0
        }
        resetEnvironment()
        ignoreOutput = true
        this@PrologOutputEnvironment.returnValues = returnValues
        val bodyNoOutput = body?.render()
        val stateCountNoOutput = stateCount
        ignoreOutput = false
        resetEnvironment()
        val bodyWithOutput = body?.render()

        val map = mapOf(
            "name" to name,
            "parameters" to operationParameters.map { it.name.lowercase() },
            "returnValues" to returnValues.map { (it as? IdentifierExpression)?.name }, // TODO: add returnValues?
            "bodyWithOutput" to bodyWithOutput,
            "body" to bodyNoOutput?.rendered?.removeSuffix(EXPRESSION_SEPARATOR),
            "resultStateCount" to stateCountNoOutput,
            "resultStateCountWithOutput" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Transition.renderSelf(): RenderResult {
        // not needed because of XTL
        return RenderResult("")
    }

    // TODO: are all operators math-operators?
    private fun isMathOperator(operator: BinaryExpressionOperator): Boolean {
        return when (operator) {
            BinaryExpressionOperator.PARALLEL_PRODUCT -> false
            else -> true
        }
    }

    private fun operator2String(operator: BinaryExpressionOperator, infiniteSet: Boolean): String {
        if (infiniteSet) {
            return when (operator) {
                BinaryExpressionOperator.ADD -> "mppcg_add"
                BinaryExpressionOperator.MINUS -> "mppcg_minus"
                BinaryExpressionOperator.MULT -> "mppcg_mult"
                BinaryExpressionOperator.DIV -> "mppcg_div"
                BinaryExpressionOperator.MOD -> "mppcg_mod"
                BinaryExpressionOperator.POW -> "mppcg_pow"
                BinaryExpressionOperator.PARALLEL_PRODUCT -> "mppcg_parallelProduct"
            }
        }
        return operator2String(operator)
    }

    override fun operator2String(operator: BinaryCollectionOperator): String {
        return when (operator) {
            BinaryCollectionOperator.INTERSECTION -> "mppcg_setIntersection"
            BinaryCollectionOperator.SUBTRACTION -> "mppcg_setSubtraction"
            BinaryCollectionOperator.UNION -> "mppcg_setUnion"
            BinaryCollectionOperator.CONCAT -> "mppcg_generalConcat"
            BinaryCollectionOperator.PRJ1 -> "mppcg_prj1"
            BinaryCollectionOperator.PRJ2 -> "mppcg_prj2"
        }
    }

    override fun operator2String(operator: BinaryFunctionOperator): String {
        return "mppcg_" + super.operator2String(operator)
    }

    override fun operator2String(operator: UnaryCollectionOperator): String {
        return when (operator) {
            UnaryCollectionOperator.MAX -> "mppcg_max"
            UnaryCollectionOperator.MIN -> "mppcg_min"
            UnaryCollectionOperator.CARD -> "mppcg_card"
            UnaryCollectionOperator.POW -> "mppcg_powerSet"
            UnaryCollectionOperator.POW1 -> "mppcg_powerSet1"
        }
    }

    override fun operator2String(operator: UnaryFunctionOperator): String {
        return "mppcg_" + super.operator2String(operator)
    }

    override fun operator2String(operator: BinarySequenceExpressionOperator): String {
        return when (operator) {
            BinarySequenceExpressionOperator.RESTRICT_FRONT -> "mppcg_sequenceRestrictFront"
            BinarySequenceExpressionOperator.RESTRICT_TAIL -> "mppcg_sequenceRestrictTail"
            BinarySequenceExpressionOperator.APPEND -> "append"
            BinarySequenceExpressionOperator.PREPEND -> "prepend"
            BinarySequenceExpressionOperator.CONCAT -> "append"
        }
    }

    override fun operator2String(operator: UnarySequenceExpressionOperator): String {
        return when (operator) {
            UnarySequenceExpressionOperator.FRONT -> "mppcg_sequenceFront"
            UnarySequenceExpressionOperator.TAIL -> "mppcg_sequenceTail"
            UnarySequenceExpressionOperator.FIRST -> "mppcg_sequenceFirst"
            UnarySequenceExpressionOperator.LAST -> "mppcg_sequenceLast"
            UnarySequenceExpressionOperator.REVERSE -> "mppcg_sequenceReverse"
        }
    }

    private fun operator2String(operator: BinaryPredicateOperator, infiniteSet: Boolean): String {
        if (infiniteSet) {
            return when (operator) {
                BinaryPredicateOperator.EQUAL -> "mppcg_equal"
                BinaryPredicateOperator.NOT_EQUAL -> "mppcg_notEqual"
                else -> operator2String(operator)
            }
        }
        return operator2String(operator)
    }

    override fun operator2String(operator: BinaryExpressionOperator): String {
        return when (operator) {
            BinaryExpressionOperator.MOD -> "mod"
            BinaryExpressionOperator.DIV -> "//"
            BinaryExpressionOperator.POW -> "mppcg_pow"
            else -> super.operator2String(operator)
        }
    }

    override fun operator2String(operator: UnaryExpressionOperator): String {
        return when (operator) {
            UnaryExpressionOperator.CONVERT_BOOLEAN -> ""
            UnaryExpressionOperator.PRED -> "mppcg_pred"
            UnaryExpressionOperator.SUCC -> "mppcg_succ"
            UnaryExpressionOperator.MINUS -> "-"
        }
    }

    override fun operator2String(operator: LogicPredicateOperator): String {
        return when (operator) {
            LogicPredicateOperator.AND -> ", "
            LogicPredicateOperator.OR -> "; "
            LogicPredicateOperator.IMPLIES -> " -> "
            LogicPredicateOperator.EQUIVALENCE -> " = "
            LogicPredicateOperator.NOT -> "\\+ "
        }
    }

    private fun isConstant(identifierExpression: IdentifierExpression): Boolean {
        if (constants.contains(identifierExpression)) return true
        if (concreteConstants.contains(identifierExpression)) return true

        val cc =
            ConcreteIdentifierExpression(identifierExpression.name, identifierExpression, identifierExpression.type)
        return concreteConstants.find { (it as? ConcreteIdentifierExpression)?.name == cc.name } != null
    }

    private fun isVariable(identifierExpression: IdentifierExpression): Boolean {
        if (variables.variables.contains(identifierExpression)) return true
        if (concreteVariables.contains(identifierExpression)) return true

        val cc =
            ConcreteIdentifierExpression(identifierExpression.name, identifierExpression, identifierExpression.type)
        return concreteVariables.find { (it as? ConcreteIdentifierExpression)?.name == cc.name } != null
    }

    override fun type2String(type: Type): String {
        return when (type) {
            MPPCG_Nat1 -> "'NAT1'"
            MPPCG_Nat -> "'NAT'"
            MPPCG_Int -> "'INT'"
            MPPCG_Integer -> "'INTEGER'"
            MPPCG_Natural -> "'NATURAL'"
            MPPCG_Natural1 -> "'NATURAL1'"
            MPPCG_Boolean -> "'BOOL'"
            is TypeSet -> type2String(type.type)
            else -> {
                println(type)
                TODO("type2String not implemented (${type::class.simpleName})")
            }
        }
    }

    private fun exprToInfo(node: MPPCGNode): Map<String, IndividualInfo> {
        if (optimize) evaluatedExpressions[node] = expr(exprCount)
        val info = mapOf(RESULT_EXPR to IndividualInfo(expr(exprCount)))
        // expr is now assigned -> increase
        exprCount++
        return info
    }
}

// TODO: rename?
private data class ExpandedExpressionList(val before: String = "", val expressions: List<String> = emptyList()) {
    companion object {
        fun of(expressions: List<Expression>): ExpandedExpressionList {
            var before = ""
            val expression = expressions.map {
                val result = it.render()

                if (it is ValueExpression || it is SetEntry) {
                    result.rendered
                } else {
                    if (result.rendered.isNotBlank() && result.containsKey(RESULT_EXPR)) {
                        before += "${result.rendered}$EXPRESSION_SEPARATOR"
                    }
                    if (result.containsKey(BEFORE)) {
                        before += result[BEFORE].info
                    }
                    if (result.info.containsKey(RESULT_EXPR)) result[RESULT_EXPR].info
                    else result.rendered
                }
            }
            return ExpandedExpressionList(before, expression)
        }
    }
}

// TODO: use expandExpression for each side
private data class ExpandedBinary(val before: String = "", val lhs: String = "", val rhs: String = "") {
    companion object {
        fun of(
            left: MPPCGNode, right: MPPCGNode
        ): ExpandedBinary {
            val lhsRendered = left.render()
            val rhsRendered = right.render()
            val before = StringBuilder()
            val lhs = createSideString(lhsRendered, before)
            val rhs = createSideString(rhsRendered, before)
            return ExpandedBinary(before.toString(), lhs, rhs)
        }

        fun of(
            left: MPPCGNode,
            right: MPPCGNode,
            differentBranches: Boolean,
            environment: PrologOutputEnvironment
        ): ExpandedBinary {
            val lhsRendered =
                environment.evaluatedExpressions.returnWithConditionedReset(differentBranches) { left.render() }
            val rhsRendered =
                environment.evaluatedExpressions.returnWithConditionedReset(differentBranches) { right.render() }

            val before = StringBuilder()
            val lhs = createSideString(lhsRendered, before)
            val rhs = createSideString(rhsRendered, before)
            return ExpandedBinary(before.toString(), lhs, rhs)
        }

        fun createSideString(rendered: RenderResult, before: StringBuilder): String {
            // TODO: replace result expr by variable to avoid misspelling
            val result = if (rendered.containsKey(RESULT_EXPR)) {
                if (rendered.rendered.isNotBlank()) {
                    before.append("${rendered.rendered}$EXPRESSION_SEPARATOR")
                }
                rendered[RESULT_EXPR].info
            } else {
                rendered.rendered
            }
            if (rendered.containsKey(BEFORE)) {
                before.append(rendered[BEFORE].info)
            }
            return result
        }
    }
}

private data class ExpandedExpression(val before: String = "", val expression: String = "") {
    companion object {
        fun of(expression: Expression): ExpandedExpression {
            val expanded = expression.render()

            var before = if (expanded.rendered.isNotBlank() && expanded.containsKey(RESULT_EXPR)) {
                "${expanded.rendered}${EXPRESSION_SEPARATOR}"
            } else {
                ""
            }

            val expr = if (expanded.containsKey(RESULT_EXPR)) {
                expanded[RESULT_EXPR].info
            } else {
                expanded.rendered
            }

            if (expanded.containsKey(BEFORE)) {
                before += expanded[BEFORE].info
            }

            return ExpandedExpression(before, expr)
        }
    }
}