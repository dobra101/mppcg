package dobra101.mppcg.environment

import dobra101.mppcg.IndividualInfo
import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import kotlin.math.max

class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    internal val optimizer = PrologOptimizer(this)

    companion object {
        const val EXPRESSION_SEPARATOR = ",\n"
    }

    var exprCount = 0
    var stateCount = 0
    var operationParameters: List<IdentifierExpression> = emptyList() // HINT: only for B
    var comprehensionSetIdentifier: List<IdentifierExpression> = emptyList() // HINT: only for B
    var lambdaExpressionIdentifier: List<IdentifierExpression> = emptyList() // HINT: only for B
    var temporaryVariables: Set<IdentifierExpression> = hashSetOf() // HINT: only for B
    var concreteConstants: List<Expression> = emptyList() // HINT: only for B
    var constants: List<Expression> = emptyList() // HINT: only for B
    var concreteVariables: List<Expression> = emptyList() // HINT: only for B
    var variables: ClassVariables = ClassVariables() // HINT: only for B
    val whileDefinitions: MutableList<String> = mutableListOf() // HINT: only for B
    var whileCount = 0 // HINT: only for B
    var currentOperation: String = "" // HINT: only for B
    var declarationStep: Boolean = true // HINT: only for B
    var ignoreOutput: Boolean = false // HINT: only for B
    var returnValues: List<Expression> = listOf() // HINT: only for B
    var ctrlStructIdentifier: List<IdentifierExpression> = listOf() // HINT: only for B

    private fun expr(name: Any): String = "Expr_$name"

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousSetCollectionNode.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        if (elements.isEmpty()) {
            return RenderResult(
                renderTemplate(templateName, mapOf("elements" to emptyList<String>()))
            )
        }

        val expanded = ExpandedExpressionList.of(elements)
        if (expanded.before.isBlank()) {
            return RenderResult(
                renderTemplate(templateName, mapOf("elements" to expanded.expressions))
            )
        }

        val map = mapOf("elements" to expanded.expressions)
        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = rendered
        // TODO: don't remove by hand
        return RenderResult(
            expanded.before.removeSuffix(EXPRESSION_SEPARATOR),
            mapOf("resultExpr" to IndividualInfo(rendered))
        )
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        val expanded = ExpandedBinary.of(left, right)

        val infiniteSet = operator == BinaryExpressionOperator.POW || (
                (left is InfiniteSet) || (right is InfiniteSet)
                        || (left is IntervalExpression) || (right is IntervalExpression)
                        || (left.type is TypeInterval) || (right.type is TypeInterval)
                        || (left is CollectionNode) || (right is CollectionNode)
                        || (left is AnonymousSetCollectionNode) || (right is AnonymousSetCollectionNode)
                )
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator, infiniteSet),
            "exprCount" to exprCount,
            "math" to isMathOperator(operator),
            "infiniteSet" to infiniteSet
        )

        val rendered = renderTemplate(map)
        return RenderResult("${expanded.before}$rendered", exprToInfo(this))
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        val map = mapOf(
            "name" to name,
            "elements" to elements.render(),
            "isParameter" to isParameter,
            "exprCount" to exprCount
        )

        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        val rendered = renderTemplate(map)
        if (isParameter) {
            val info = mapOf("resultExpr" to IndividualInfo(expr(exprCount)))
            // expr is now assigned -> increase
            exprCount++
            return RenderResult(rendered, info)
        }
        return RenderResult(rendered)
    }

    // HINT: same as SetEntry
    override fun EnumEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        // TODO: not hardcoded and not always
        if (operationParameters.contains(this)
            || comprehensionSetIdentifier.contains(this)
            || lambdaExpressionIdentifier.contains(this)
        ) {
            if (optimize) optimizer.evaluated[this] = expr(name)
            return RenderResult(expr(name))
        }

        // TODO: not hardcoded and not always
        if (temporaryVariables.contains(this)) {
            if (optimize) optimizer.evaluated[this] = expr("tmp_$name")
            return RenderResult(expr("tmp_$name"))
        }

        if (ctrlStructIdentifier.contains(this)) {
            if (optimize) optimizer.evaluated[this] = expr("q_$name")
            return RenderResult(expr("q_$name"))
        }

        // TODO: not hardcoded and not always
        val rendered =
            if (isConstant(this)) {
                // use constant prefix
                val cc = ConcreteIdentifierExpression(name, ValueExpression(), type)
                "${cc.name}(${expr(exprCount)})"
            } else {
                val map = mapOf(
                    "name" to name,
                    "stateCount" to stateCount,
                    "exprCount" to exprCount
                )
                renderTemplate(map)
            }
        return RenderResult(rendered, exprToInfo(this))
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs
        )
        return RenderResult(renderTemplate(map), info = mapOf("before" to IndividualInfo(expanded.before)))
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
        val map = if (type is TypeBoolean && (type as TypeBoolean).value != null) {
            mapOf("value" to ((type as TypeBoolean).value == BooleanValue.TRUE).toString())
        } else {
            mapOf("value" to value)
        }

        return RenderResult(renderTemplate(map))
    }


    /* ---------- PREDICATES ---------- */
    // HINT: SAME AS LOGIC PREDICATE (except optimization)
    override fun BinaryPredicate.renderSelf(): RenderResult {
        if (optimize) optimizer.renderOptimized(this)?.let { return it }

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

        val infiniteSet =
            (left is IntervalExpression || right is IntervalExpression || left.type is TypeCollection || right.type is TypeCollection)
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
            if (right !is BinaryPredicate) return false
            return true
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
        if (optimize) {
            val evaluated = optimizer.getCopyOfEvaluated()
            val map = mapOf(
                "predicate" to predicate.render(),
                "operator" to operator2String(operator)
            )
            optimizer.evaluated = evaluated
            return RenderResult(renderTemplate(map))
        }

        val map = mapOf(
            "predicate" to predicate.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ValuePredicate.renderSelf(): RenderResult {
        val map = if (type is TypeBoolean) {
            mapOf("value" to (type as TypeBoolean).value?.name?.lowercase())
        } else {
            mapOf("value" to value)
        }
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
        val map = mapOf(
            "identifier" to identifier,
            "rhs" to expandedRhs.expression,
            "stateCount" to stateCount,
            "resultStateCount" to ++stateCount
        )
        val rendered = renderTemplate(map)

        if (optimize && !temporaryVariables.contains(left)) {
            optimizer.evaluated[left] = expandedRhs.expression
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

        val thenRendered = then.render()

        if (elseSubstitution == null) {
            addStateAndExprCountFixingElseBranch(stateCountBefore, exprCountBefore, map, thenRendered)
        } else {
            val stateCountAfterThen = stateCount
            val exprCountAfterThen = exprCount
            stateCount = stateCountBefore
            exprCount = exprCountBefore

            val elseRendered = elseSubstitution!!.render()

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
            val evaluated = if (optimize) optimizer.getCopyOfEvaluated() else hashMapOf()
            optimizer.evaluated = hashMapOf()
            val conditionRendered = condition.render()
            optimizer.evaluated = hashMapOf()
            val map = mapOf(
                "name" to currentOperation,
                "count" to whileCount,
                "condition" to conditionRendered,
                "body" to body.render(),
                "lastState" to stateCount
            )
            optimizer.evaluated = evaluated
            return map
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
        whileDefinitions.add(renderTemplate("whileDefinition", whileDefinitionMap))
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
            "type" to (type as TypeFunction).type,
            "mapType" to mapType
        )
        return RenderResult(renderTemplate(map), info = mapOf("before" to IndividualInfo(expanded.before)))
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered", exprToInfo(this))
    }

    // TODO: reuse BinaryExpression?
    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered", exprToInfo(this))
    }

    override fun BinarySequenceExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        // TODO: refactor append/prepend
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to if (operator == BinarySequenceExpressionOperator.APPEND) "[${expanded.rhs}]" else expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered", exprToInfo(this))
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val before1 = ExpandedExpression.of(expression)
        val before2 = ExpandedExpressionList.of(parameters)

        val map = mapOf(
            "expression" to before1.expression,
            "parameters" to before2.expressions,
            "exprCount" to exprCount
        )

        val before = "${before1.before}${before2.before}"
        val rendered = renderTemplate(map)

        return RenderResult("$before$rendered", exprToInfo(this))
    }

    // TODO: optimizer.getCopyOfEvaluated
    override fun ComprehensionSet.renderSelf(): RenderResult {
        val evaluated = if (optimize) optimizer.getCopyOfEvaluated() else hashMapOf()
        comprehensionSetIdentifier = comprehensionSetIdentifier + identifiers.filterIsInstance<IdentifierExpression>()
        val map = mapOf(
            "identifiers" to identifiers.render(),
            "predicates" to predicates.render(),
            "exprCount" to exprCount
        )
        comprehensionSetIdentifier =
            comprehensionSetIdentifier - identifiers.filterIsInstance<IdentifierExpression>().toSet()
        if (optimize) optimizer.evaluated = evaluated
        return RenderResult(renderTemplate(map), exprToInfo(this))
    }

    override fun ConcreteIdentifierExpression.renderSelf(): RenderResult {
        if (declarationStep) {
            if (optimize) optimizer.evaluated = hashMapOf()
            stateCount = 0
            exprCount = 0
            temporaryVariables = hashSetOf()
        }

        if (declarationStep) {
            val renderedValue = value.render()
            val before = if (renderedValue.containsKey("before")) renderedValue["before"].info else ""
            val map = mapOf(
                "name" to name,
                "before" to before.removeSuffix(EXPRESSION_SEPARATOR),
                "value" to renderedValue.rendered,
                "interval" to (value is IntervalExpression),
                "inline" to (!renderedValue.rendered.contains(EXPRESSION_SEPARATOR) && value !is ComprehensionSet && value !is LambdaExpression), // TODO: type checks needed?
                "exprCount" to exprCount - 1 // last assigned expression
            )
            return RenderResult(renderTemplate("concreteIdentifierDeclaration", map))
        }
        val map = mapOf(
            "name" to name,
            "exprCount" to exprCount
        )
        return RenderResult(renderTemplate(map), exprToInfo(this))
    }

    override fun Couple.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(from, to)
        val map = mapOf(
            "from" to expanded.lhs,
            "to" to expanded.rhs
        )

        return RenderResult(renderTemplate(map), info = mapOf("before" to IndividualInfo(expanded.before)))
    }

    override fun GeneralSumOrProductExpression.renderSelf(): RenderResult {
        val evaluated = if (optimize) optimizer.getCopyOfEvaluated() else hashMapOf()
        ctrlStructIdentifier = ctrlStructIdentifier + identifiers

        val map = mapOf(
            "identifiers" to identifiers.render(),
            "predicate" to predicate.render(),
            "expression" to if (expression !is IdentifierExpression) expression.render() else null,
            "isSum" to (operation == SumOrProductOperation.SUM),
            "exprCount" to exprCount
        )

        ctrlStructIdentifier = ctrlStructIdentifier - identifiers.toSet()
        if (optimize) optimizer.evaluated = evaluated

        return RenderResult(renderTemplate(map), exprToInfo(this))
    }

    override fun InfiniteSet.renderSelf(): RenderResult {
        return RenderResult(renderTemplate(mapOf("type" to type2String(type))))
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
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        val expanded = ExpandedExpression.of(collection)

        val map = mutableMapOf(
            "collection" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        return RenderResult("${expanded.before}${renderTemplate(map)}", exprToInfo(this))
    }

    override fun UnaryExpression.renderSelf(): RenderResult {
        // TODO: optimize minus
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val renderedValue = if (optimize && optimizer.evaluated.contains(value)) null else value.render()
        val resultAt =
            if (optimize && optimizer.evaluated.contains(value)) optimizer.evaluated[value] else expr(exprCount - 1)

        val map = mapOf(
            "value" to renderedValue,
            "operator" to operator2String(operator),
            "convertBoolean" to (operator == UnaryExpressionOperator.CONVERT_BOOLEAN),
            "isMinus" to (operator == UnaryExpressionOperator.MINUS),
            "isMinusInline" to (value is ValueExpression || value is ConcreteIdentifierExpression),
            "resultAt" to resultAt,
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        return RenderResult(rendered, exprToInfo(this))
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedExpression.of(expression)

        val map = mapOf(
            "expression" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered", exprToInfo(this))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        val checkInvs = List(predicates.size) { idx ->
            if (optimize) optimizer.evaluated = hashMapOf()
            exprCount = 0
            stateCount = 0
            val map = mapOf(
                "body" to predicates[idx].render(),
                "idx" to idx
            )
            renderTemplate(map)
        }

        val renderedCheckInvs = renderTemplate("invariants", mapOf("list" to checkInvs))

        val map = mapOf(
            "invariants" to renderedCheckInvs,
            "indices" to predicates.indices.map { it }
        )

        return RenderResult(renderTemplate("props", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        val evaluated = if (optimize) optimizer.getCopyOfEvaluated() else hashMapOf()
        ctrlStructIdentifier = ctrlStructIdentifier + identifier
        val map = mapOf(
            "identifier" to identifier.render(),
            "predicate" to predicate.render(),
            "quantification" to quantification?.render(),
            "universalQuantifier" to (type == QuantifierType.FORALL)
        )
        ctrlStructIdentifier = ctrlStructIdentifier - identifier.toSet()
        if (optimize) optimizer.evaluated = evaluated
        return RenderResult(renderTemplate(map))
    }

    override fun Initialization.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()
        val map = mapOf(
            "body" to substitutions.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun ParallelSubstitution.renderSelf(): RenderResult {
        temporaryVariables = needTempVar

        // remove temporary variables from previous evaluated to use temp vars
        if (optimize) temporaryVariables.forEach { optimizer.evaluated.remove(it) }

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

        return RenderResult(renderTemplate(map))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Precondition.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()

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
        if (optimize) optimizer.evaluated = hashMapOf()
        declarationStep = true
        this@PrologOutputEnvironment.concreteConstants = concreteConstants
        this@PrologOutputEnvironment.constants = constants
        stateCount = 0
        exprCount = 0
        operationParameters = parameters.map { it as IdentifierExpression }
        temporaryVariables = hashSetOf()
        this@PrologOutputEnvironment.concreteVariables = concreteVariables
        this@PrologOutputEnvironment.variables = variables

        val concreteConstantsRendered = concreteConstants.render()
        declarationStep = false

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstantsRendered,
//            "properties" to properties?.render(), // TODO: use properties
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization.render(),
            "invariant" to invariant.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "whileDefinitions" to whileDefinitions
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Operation.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()
        stateCount = 0
        exprCount = 0
        operationParameters = parameters.map { it as IdentifierExpression }
        temporaryVariables = hashSetOf()
        currentOperation = name
        whileCount = 0

        ignoreOutput = true
        this@PrologOutputEnvironment.returnValues = returnValues
        val bodyNoOutput = body?.render()
        val stateCountNoOutput = stateCount
        ignoreOutput = false
        if (optimize) optimizer.evaluated = hashMapOf()
        stateCount = 0
        exprCount = 0
        operationParameters = parameters.map { it as IdentifierExpression }
        temporaryVariables = hashSetOf()
        currentOperation = name
        whileCount = 0
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
            BinaryExpressionOperator.ADD -> true
            BinaryExpressionOperator.MINUS -> true
            BinaryExpressionOperator.MULT -> true
            BinaryExpressionOperator.DIV -> true
            BinaryExpressionOperator.MOD -> true
            BinaryExpressionOperator.POW -> true
            BinaryExpressionOperator.PARALLEL_PRODUCT -> false
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
            BinaryCollectionOperator.CONCAT -> "mppcg_concat"
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
            BinarySequenceExpressionOperator.CONCAT -> "concat"
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

    override fun operator2String(operator: LogicPredicateOperator): String {
        return when (operator) {
            LogicPredicateOperator.AND -> ", "
            LogicPredicateOperator.OR -> "; "
            LogicPredicateOperator.IMPLIES -> " -> "
            LogicPredicateOperator.EQUIVALENCE -> " = "
            LogicPredicateOperator.NOT -> "\\+ "
        }
    }

    internal fun isConstant(identifierExpression: IdentifierExpression): Boolean {
        if (constants.contains(identifierExpression)) return true
        if (concreteConstants.contains(identifierExpression)) return true

        val cc = ConcreteIdentifierExpression(identifierExpression.name, ValueExpression(), identifierExpression.type)
        return concreteConstants.find { (it as? ConcreteIdentifierExpression)?.name == cc.name } != null
    }

    internal fun isVariable(identifierExpression: IdentifierExpression): Boolean {
        if (variables.variables.contains(identifierExpression)) return true
        if (concreteVariables.contains(identifierExpression)) return true

        val cc = ConcreteIdentifierExpression(identifierExpression.name, ValueExpression(), identifierExpression.type)
        return concreteVariables.find { (it as? ConcreteIdentifierExpression)?.name == cc.name } != null
    }

    override fun type2String(type: Type?): String {
        return when (type) {
            is TypeNat1 -> "'NAT1'"
            is TypeNat -> "'NAT'"
            is TypeInt -> "'INT'"
            is TypeInteger -> "'INTEGER'"
            is TypeNatural -> "'NATURAL'"
            is TypeSet -> type2String(type.type)
            is TypeBoolean -> "'BOOL'"
            else -> TODO("Infinite Set not implemented (${type!!::class.simpleName})")
        }
    }

    private fun exprToInfo(node: MPPCGNode): Map<String, IndividualInfo> {
        if (optimize) optimizer.evaluated[node] = expr(exprCount)
        val info = mapOf("resultExpr" to IndividualInfo(expr(exprCount)))
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
                    if (result.rendered.isNotBlank() && result.containsKey("resultExpr")) {
                        before += "${result.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                    }
                    if (result.containsKey("before")) {
                        before += result["before"].info
                    }
                    if (result.info.containsKey("resultExpr")) result["resultExpr"].info
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
            return of(left, right, false, null)
        }

        fun of(
            left: MPPCGNode,
            right: MPPCGNode,
            differentBranches: Boolean,
            environment: PrologOutputEnvironment?
        ): ExpandedBinary {
            // true copy of map
            val evaluated =
                if (environment?.optimize == true) environment.optimizer.getCopyOfEvaluated() else hashMapOf()

            val lhsRendered = left.render()
            if (differentBranches && environment != null) environment.optimizer.evaluated = evaluated
            val rhsRendered = right.render()
            if (differentBranches && environment != null) environment.optimizer.evaluated = evaluated

            var before = ""
            val lhs: String
            val rhs: String
            if (lhsRendered.containsKey("resultExpr")) { // TODO: replace result expr by variable to avoid misspelling
                lhs = lhsRendered["resultExpr"].info
                if (lhsRendered.rendered.isNotBlank()) {
                    before += "${lhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                }
            } else {
                lhs = lhsRendered.rendered
            }
            if (lhsRendered.containsKey("before")) {
                before += lhsRendered["before"].info
            }

            if (rhsRendered.containsKey("resultExpr")) {
                rhs = rhsRendered["resultExpr"].info
                if (rhsRendered.rendered.isNotBlank()) {
                    before += "${rhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                }
            } else {
                rhs = rhsRendered.rendered
            }
            if (rhsRendered.containsKey("before")) {
                before += rhsRendered["before"].info
            }
            return ExpandedBinary(before, lhs, rhs)
        }
    }
}

private data class ExpandedExpression(val before: String = "", val expression: String = "") {
    companion object {
        fun of(expression: Expression): ExpandedExpression {
            val expanded = expression.render()

            var before = if (expanded.rendered.isNotBlank() && expanded.containsKey("resultExpr")) {
                "${expanded.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
            } else {
                ""
            }

            val expr = if (expanded.containsKey("resultExpr")) {
                expanded["resultExpr"].info
            } else {
                expanded.rendered
            }

            if (expanded.containsKey("before")) {
                before += expanded["before"].info
            }

            return ExpandedExpression(before, expr)
        }
    }
}