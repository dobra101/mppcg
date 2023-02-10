package dobra101.mppcg.environment

import dobra101.mppcg.IndividualInfo
import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.AssignSubstitution
import dobra101.mppcg.node.substitution.IfSubstitution
import dobra101.mppcg.node.substitution.ParallelSubstitution
import dobra101.mppcg.node.substitution.SequenceSubstitution
import kotlin.math.max

class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    private val optimizer = PrologOptimizer(this)

    companion object {
        const val EXPRESSION_SEPARATOR = ",\n"
    }

    var exprCount = 0
    var stateCount = 0
    var operationParameters: List<IdentifierExpression> = emptyList() // HINT: only for B
    var usedBMethods: HashSet<BMethod> = hashSetOf() // HINT: only for B

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
        // TODO: dont remove by hand
        return RenderResult(
            expanded.before.removeSuffix(EXPRESSION_SEPARATOR),
            mapOf("resultExpr" to IndividualInfo(rendered))
        )
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount,
            "math" to isMathOperator(operator)
        )

        val rendered = renderTemplate(map)
        val info = mapOf("resultExpr" to IndividualInfo("Expr_$exprCount")) // TODO: map to Int?

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"
        return RenderResult("${expanded.before}$rendered", info)
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "elements" to elements.render(),
            "isParameter" to isParameter,
            "exprCount" to exprCount
        )

        val rendered = renderTemplate(map)
        if (isParameter) {
            val info = mapOf("resultExpr" to IndividualInfo("Expr_$exprCount")) // TODO: map to Int?
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
        if (operationParameters.contains(this)) {
            return RenderResult("Expr_$name")
        }

        val map = mapOf(
            "name" to name,
            "stateCount" to stateCount,
            "exprCount" to exprCount
        )

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"
        val info = mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}")) // TODO: map to Int?

        return RenderResult(renderTemplate(map), info)
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs
        )

        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered")

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

        val expanded = ExpandedBinary.of(left, right)

        val prefixOperators = listOf(
            BinaryPredicateOperator.MEMBER,
            BinaryPredicateOperator.NOT_MEMBER,
            BinaryPredicateOperator.SUBSET
        )

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "prefixOperator" to prefixOperators.contains(operator)
        )
        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered")
    }

    // HINT: SAME AS BINARY PREDICATE (except optimization)
    override fun BinaryLogicPredicate.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)

        val lineBreaksTotal = expanded.lhs.count { it == '\n' } + expanded.rhs.count { it == '\n' }
        if (lineBreaksTotal == 0) {
            println(left)
            println(right)
            println("======")
        }
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "addParentheses" to (operator == LogicPredicateOperator.IMPLIES),
            "inline" to (lineBreaksTotal == 0 && left is BinaryPredicate && right is BinaryPredicate)
        )

        val rendered = renderTemplate(map)

        return RenderResult("${expanded.before}$rendered")
    }

    override fun UnaryLogicPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "predicate" to predicate.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }


    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        val identifier = (lhs[0] as IdentifierExpression).name // TODO: when more than one identifier?

        // don't expand collection entries
        if (rhs.size == 1 && (rhs[0] is CollectionEntry || rhs[0] is AnonymousCollectionNode)) {
            val map = mapOf(
                "identifier" to identifier,
                "rhs" to rhs[0].render(),
                "stateCount" to stateCount,
                "resultStateCount" to ++stateCount
            )
            return RenderResult(renderTemplate(map))
        }

        val expandedRhs = ExpandedExpressionList.of(rhs) // TODO: dont expand if rhs is CollectionEntry
        val map = mapOf(
            "identifier" to identifier,
            "rhs" to expandedRhs.expressions[0], // TODO: more than one entry?
            "stateCount" to stateCount,
            "resultStateCount" to ++stateCount
        )
        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[lhs[0]] = expandedRhs.expressions[0]
        return RenderResult("${expandedRhs.before}$rendered")
    }

    override fun IfSubstitution.renderSelf(): RenderResult {
        val map = mutableMapOf(
            "condition" to condition.render(),
            "elseIf" to elseIf.render()
        )

        val stateCountBefore = stateCount
        val exprCountBefore = exprCount

        val thenRendered = then.render()
        var before = ""

        if (elseSubstitution == null) {
            val mainBranch = StringBuilder("")
            // TODO: extract to function
            // set the stateCount and exprCount for the main branch
            if (stateCountBefore < stateCount) {
                mainBranch.append(
                    renderTemplate(
                        "updateStateCount", mapOf(
                            "newCount" to stateCount,
                            "oldCount" to stateCountBefore
                        )
                    )
                )
                    .append(EXPRESSION_SEPARATOR)
            }

            if (exprCountBefore < exprCount) {
                mainBranch.append(
                    renderTemplate(
                        "updateExprCount", mapOf(
                            "newCount" to exprCount,
                            "oldCount" to exprCountBefore
                        )
                    )
                )
                    .append(EXPRESSION_SEPARATOR)
            }
            map["then"] = thenRendered
            before = mainBranch.toString()
        } else {
            // TODO: extract to function

            val stateCountAfterThen = stateCount
            val exprCountAfterThen = exprCount
            stateCount = stateCountBefore
            exprCount = exprCountBefore

            val elseRendered = elseSubstitution!!.render()
            val stateCountAfterElse = stateCount
            val exprCountAfterElse = exprCount

            stateCount = max(stateCountAfterThen, stateCountAfterElse)
            exprCount = max(exprCountAfterThen, exprCountAfterElse)

            // set the stateCount and exprCount for the main branch
            val thenStringBuilder = StringBuilder(thenRendered.rendered)
            val elseStringBuilder = StringBuilder(elseRendered.rendered)

            // stateCount
            if (stateCountAfterThen < stateCountAfterElse) {
                thenStringBuilder.append(EXPRESSION_SEPARATOR)
                    .append(
                        renderTemplate(
                            "updateStateCount", mapOf(
                                "newCount" to stateCount,
                                "oldCount" to stateCountAfterThen
                            )
                        )
                    )
            } else if (stateCountAfterThen > stateCountAfterElse) {
                elseStringBuilder.append(EXPRESSION_SEPARATOR)
                    .append(
                        renderTemplate(
                            "updateStateCount", mapOf(
                                "newCount" to stateCount,
                                "oldCount" to stateCountAfterElse
                            )
                        )
                    )
            }

            // exprCount
            if (exprCountAfterThen < exprCountAfterElse) {
                thenStringBuilder.append(EXPRESSION_SEPARATOR)
                    .append(
                        renderTemplate(
                            "updateExprCount", mapOf(
                                "newCount" to exprCount - 1,
                                "oldCount" to exprCountAfterThen - 1
                            )
                        )
                    )
            } else if (exprCountAfterThen > exprCountAfterElse) {
                elseStringBuilder.append(EXPRESSION_SEPARATOR)
                    .append(
                        renderTemplate(
                            "updateExprCount", mapOf(
                                "newCount" to exprCount - 1,
                                "oldCount" to exprCountAfterElse - 1
                            )
                        )
                    )
            }

            map["then"] = RenderResult(thenStringBuilder.toString(), thenRendered.info)
            map["elseSubstitution"] = RenderResult(elseStringBuilder.toString(), elseRendered.info)
        }

        return RenderResult("$before${renderTemplate(map)}")
    }

    override fun SequenceSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "type" to (type as TypeFunction).type,
            "mapType" to mapType
        )

        return RenderResult(renderTemplate(map))
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

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"
        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}")))
    }

    // TODO: reuse BinaryExpression?
    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        usedBMethods.add(operator)

        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"
        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}")))
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

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"
        return RenderResult("$before$rendered", mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}")))
    }

    override fun Couple.renderSelf(): RenderResult {
        val map = mapOf(
            "list" to list.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun InfiniteSet.renderSelf(): RenderResult {
        TODO("Infinite Set not implemented")
    }

    override fun UnaryCollectionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        val expanded = ExpandedExpression.of(collection)

        val map = mutableMapOf(
            "collection" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"

        return RenderResult(
            "${expanded.before}${renderTemplate(map)}",
            mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}"))
        )
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        usedBMethods.add(operator)

        val expanded = ExpandedExpression.of(expression)

        val map = mapOf(
            "expression" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = "Expr_$exprCount"

        return RenderResult(
            "${expanded.before}$rendered",
            mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}"))
        )
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
        val map = mapOf(
            "identifier" to identifier.renderSelf(),
            "predicate" to predicate.render(),
            "type" to type
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Initialization.renderSelf(): RenderResult {
        val map = mapOf(
            "body" to substitutions.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun ParallelSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Precondition.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()

        val map = mapOf(
            "predicate" to predicate.render().rendered,
            "substitution" to substitution.render().rendered
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
        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
//            "properties" to properties?.render(), // TODO: use properties
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization?.render(),
            "invariant" to invariant.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "methods" to usedBMethods.render()
        )

        logger.info("Used methods: $usedBMethods")

        return RenderResult(renderTemplate(map))
    }

    override fun Operation.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()
        stateCount = 0
        exprCount = 0
        operationParameters = parameters.map { it as IdentifierExpression }

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
//            "returnValues" to returnValues.render(), // TODO: add returnValues?
            "body" to body.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Transition.renderSelf(): RenderResult {
        // not needed because of XTL
        return RenderResult("")
    }

    override fun type2String(type: Type?): String {
        TODO("Not yet implemented")
    }

    // TODO: are all operators math-operators?
    private fun isMathOperator(operator: BinaryExpressionOperator): Boolean {
        return when (operator) {
            BinaryExpressionOperator.ADD -> true
            BinaryExpressionOperator.MINUS -> true
            BinaryExpressionOperator.MULT -> true
            BinaryExpressionOperator.DIV -> true
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

    // HINT: input language specific
    private fun HashSet<BMethod>.render(): List<String> {
        return map {
            when (it) {
                is UnaryFunctionOperator -> it.render()
                is BinaryFunctionOperator -> it.render()
                else -> throw EnvironmentException("Rendering of BMethod $it is not implemented")
            }
        }
    }

    // HINT: input language specific
    private fun UnaryFunctionOperator.render(): String {
        return when (this) {
            UnaryFunctionOperator.DOMAIN -> renderTemplate("domain")
            UnaryFunctionOperator.RANGE -> renderTemplate("range")
            UnaryFunctionOperator.REVERSE -> renderTemplate("reverse")
        }
    }

    // HINT: input language specific
    private fun BinaryFunctionOperator.render(): String {
        return when (this) {
            BinaryFunctionOperator.DOMAIN_RESTRICTION -> renderTemplate("domainRestriction")
            BinaryFunctionOperator.DOMAIN_SUBTRACTION -> renderTemplate("domainSubtraction")
            BinaryFunctionOperator.IMAGE -> renderTemplate("image")
            BinaryFunctionOperator.OVERWRITE -> renderTemplate("overwrite")
            BinaryFunctionOperator.RANGE_RESTRICTION -> renderTemplate("rangeRestriction")
            BinaryFunctionOperator.RANGE_SUBTRACTION -> renderTemplate("rangeSubtraction")
        }
    }
}

// TODO: rename?
private data class ExpandedExpressionList(val before: String = "", val expressions: List<String> = emptyList()) {
    companion object {
        fun of(expressions: List<Expression>): ExpandedExpressionList {
            var before = ""
            val expression = expressions.map {
                val result = it.render()

                if ((it is IdentifierExpression) || it is ValueExpression || it is SetEntry) {
                    result.rendered
                } else {
                    if (result.rendered.isNotBlank() && result.containsKey("resultExpr")) {
                        before += "${result.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                    }
                    if (result.info.containsKey("resultExpr")) result["resultExpr"].info
                    else result.rendered
                }
            }
            return ExpandedExpressionList(before, expression)
        }
    }
}

private data class ExpandedBinary(val before: String = "", val lhs: String = "", val rhs: String = "") {
    companion object {
        fun of(left: MPPCGNode, right: MPPCGNode): ExpandedBinary {
            val lhsRendered = left.render()
            val rhsRendered = right.render()

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

            if (rhsRendered.containsKey("resultExpr")) {
                rhs = rhsRendered["resultExpr"].info
                if (rhsRendered.rendered.isNotBlank()) {
                    before += "${rhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                }
            } else {
                rhs = rhsRendered.rendered
            }
            return ExpandedBinary(before, lhs, rhs)
        }
    }
}

private data class ExpandedExpression(val before: String = "", val expression: String = "") {
    companion object {
        fun of(expression: Expression): ExpandedExpression {
            val expanded = expression.render()

            val before = if (expanded.rendered.isNotBlank() && expanded.containsKey("resultExpr")) {
                "${expanded.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
            } else {
                ""
            }

            val expr = if (expanded.containsKey("resultExpr")) {
                expanded["resultExpr"].info
            } else {
                expanded.rendered
            }

            return ExpandedExpression(before, expr)
        }
    }
}