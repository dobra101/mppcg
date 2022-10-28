package dobra101.mppcg.environment

import dobra101.mppcg.IndividualInfo
import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.SetEntry
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.BinaryPredicate
import dobra101.mppcg.node.predicate.LogicPredicate
import dobra101.mppcg.node.predicate.LogicPredicateOperator
import dobra101.mppcg.node.substitution.AssignSubstitution

class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    private val optimizer = PrologOptimizer(this)

    companion object {
        const val EXPRESSION_SEPARATOR = ",\n"
    }

    var exprCount = 0
    var stateCount = 0

    /* ---------- EXPRESSIONS ---------- */
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

    override fun IdentifierExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "stateCount" to stateCount,
            "exprCount" to exprCount++
        )
        val rendered = stRender("identifierExpression", map)
        val info = mapOf("resultExpr" to IndividualInfo("Expr_${exprCount - 1}")) // TODO: map to Int?

        return RenderResult(rendered, info)
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
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

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator)
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
        val expandedRhs = ExpandedExpression.of(rhs)

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
        TODO("Not yet implemented")
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

    override fun Initialization.renderSelf(): RenderResult {
        val map = mapOf(
            "body" to substitutions.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(stRender("initialization", map))
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
        TODO("Not yet implemented")
    }

    override fun Machine.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
            "properties" to properties?.render(),
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

                if (it is IdentifierExpression || it is ValueExpression || it is SetEntry) {
                    result.rendered
                } else {
                    before += "${result.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
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
                before += "${lhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
            } else {
                lhs = lhsRendered.rendered
            }

            if (rhsRendered.containsKey("resultExpr")) {
                rhs = rhsRendered["resultExpr"].info
                before += "${rhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
            } else {
                rhs = rhsRendered.rendered
            }
            return ExpandedBinary(before, lhs, rhs)
        }
    }
}