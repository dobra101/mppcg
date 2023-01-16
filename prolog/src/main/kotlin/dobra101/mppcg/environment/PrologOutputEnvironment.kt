package dobra101.mppcg.environment

import dobra101.mppcg.IndividualInfo
import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.AssignSubstitution
import dobra101.mppcg.node.substitution.ParallelSubstitution

class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    private val optimizer = PrologOptimizer(this)

    companion object {
        const val EXPRESSION_SEPARATOR = ",\n"
    }

    private var exprCount = 0
    var stateCount = 0
    var operationParameters: List<IdentifierExpression> = emptyList()

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousSetCollectionNode.renderSelf(): RenderResult {
        if (elements.isEmpty()) {
            return RenderResult(
                stRender("anonymousSetCollectionExpression", mapOf("elements" to emptyList<String>()))
            )
        }

        val expanded = ExpandedExpression.of(elements)

        val map = mapOf(
            "elements" to expanded.expressions
        )

        val rendered = stRender("anonymousSetCollectionExpression", map)

        return RenderResult(expanded.before, mapOf("resultExpr" to IndividualInfo(rendered)))
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount,
            "math" to isMathOperator(operator)
        )
        val rendered = stRender("binaryExpression", map)
        val info = mapOf("resultExpr" to IndividualInfo("Expr_$exprCount")) // TODO: map to Int?

        return RenderResult("${expanded.before}$rendered", info)
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "elements" to elements.render()
        )

        return RenderResult(stRender("enumCollectionExpression", map))
    }

    // HINT: same as SetEntry
    override fun EnumEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(stRender("enumEntryExpression", map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        // TODO: not hardcoded and not always
        if (operationParameters.contains(this)) {
            return RenderResult("Expr_$name")
        }

        val map = mapOf(
            "name" to name,
            "stateCount" to stateCount,
            "exprCount" to exprCount++
        )
        val rendered = stRender("identifierExpression", map)

        // TODO: move before map to avoid subtraction
        val info = mapOf("resultExpr" to IndividualInfo("Expr_${exprCount - 1}")) // TODO: map to Int?

        return RenderResult(rendered, info)
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun SetCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: same as EnumEntry
    override fun SetEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(stRender("setEntryExpression", map))
    }

    // HINT: Same for Java and Prolog
    override fun ValueExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "value" to value
        )

        return RenderResult(stRender("valueExpression", map))
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
        val rendered = stRender("binaryPredicate", map)

        return RenderResult("${expanded.before}$rendered")
    }

    // HINT: SAME AS BINARY PREDICATE (except optimization)
    override fun LogicPredicate.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator)
        )
        val rendered = stRender("logicPredicate", map)

        return RenderResult("${expanded.before}$rendered")
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
            return RenderResult(stRender("assignSubstitution", map))
        }

        val expandedRhs = ExpandedExpression.of(rhs) // TODO: dont expand if rhs is CollectionEntry
        val map = mapOf(
            "identifier" to identifier,
            "rhs" to expandedRhs.expressions[0], // TODO: more than one entry?
            "stateCount" to stateCount,
            "resultStateCount" to ++stateCount
        )
        val rendered = stRender("assignSubstitution", map)

        return RenderResult("${expandedRhs.before}$rendered")
    }


    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "type" to type.type,
            "mapType" to mapType
        )

        return RenderResult(stRender("function", map))
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = stRender("binaryCollectionExpression", map)

        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}")))
    }

    // TODO: reuse BinaryExpression?
    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = stRender("binaryFunctionExpression", map)

        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}")))
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "expression" to expression.render(),
            "parameters" to parameters.render()
        )

        return RenderResult(stRender("callFunction", map))
    }

    override fun Couple.renderSelf(): RenderResult {
        val map = mapOf(
            "list" to list.render()
        )

        return RenderResult(stRender("couple", map))
    }

    override fun DomainFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "expression" to expression.render()
        )

        return RenderResult(stRender("domainFunction", map))
    }

    override fun RangeFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "expression" to expression.render()
        )

        return RenderResult(stRender("rangeFunction", map))
    }

    override fun ReverseFunctionExpression.renderSelf(): RenderResult {
        // TODO: expand single expression
        val before = expression.render()

        val map = mapOf(
            "expression" to before.info["resultExpr"]?.info, // TODO: dont use hardcoded naming
            "exprCount" to exprCount
        )

        val rendered = stRender("reverseFunction", map)

        return RenderResult(
            "${before.rendered},\n$rendered",
            mapOf("resultExpr" to IndividualInfo("Expr_${exprCount++}"))
        )
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        stateCount = 0
        exprCount = 0

        val map = mapOf(
            "body" to predicate.render()
        )

        return RenderResult(stRender("invariant", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "identifier" to identifier.renderSelf(),
            "predicate" to predicate.render(),
            "type" to type
        )

        return RenderResult(stRender("quantifier", map))
    }

    override fun Initialization.renderSelf(): RenderResult {
        val map = mapOf(
            "body" to substitutions.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(stRender("initialization", map))
    }

    override fun ParallelSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(stRender("parallelSubstitution", map))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Precondition.renderSelf(): RenderResult {
        val map = mapOf(
            "predicate" to predicate.render().rendered,
            "substitution" to substitution.render().rendered
        )

        return RenderResult(stRender("precondition", map))
    }

    override fun Select.renderSelf(): RenderResult {
        // TODO: add when, else
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then.render()
        )

        return RenderResult(stRender("select", map))
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
            "invariant" to invariant?.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render()
        )

        return RenderResult(stRender("machine", map))
    }

    override fun Operation.renderSelf(): RenderResult {
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

        return RenderResult(stRender("operation", map))
    }

    override fun Transition.renderSelf(): RenderResult {
        // not needed because of XTL
        return RenderResult("")
    }

    override fun type2String(type: Type): String {
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
            LogicPredicateOperator.AND -> ","
            LogicPredicateOperator.OR -> ";"
            LogicPredicateOperator.IMPLIES -> "=>"
        }
    }
}

// TODO: rename?
private data class ExpandedExpression(val before: String = "", val expressions: List<String> = emptyList()) {
    companion object {
        fun of(expressions: List<Expression>): ExpandedExpression {
            var before = ""
            val expression = expressions.map {
                val result = it.render()

                if ((it is IdentifierExpression) || it is ValueExpression || it is SetEntry) {
                    result.rendered
                } else {
                    if (result.rendered.isNotBlank()) {
                        before += "${result.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                    }
                    if (result.info.containsKey("resultExpr")) result["resultExpr"].info
                    else ""
                }
            }
            return ExpandedExpression(before, expression)
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