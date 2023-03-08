package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.environment.PrologOutputEnvironment.Companion.EXPRESSION_SEPARATOR
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.TypeBoolean
import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.collection.CollectionEntry
import dobra101.mppcg.node.collection.SetEntry
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.expression.ValueExpression
import dobra101.mppcg.node.predicate.BinaryPredicate
import dobra101.mppcg.node.predicate.BinaryPredicateOperator

class PrologOptimizer(private val environment: PrologOutputEnvironment) {

    var evaluated: HashMap<MPPCGNode, String> = hashMapOf()

    /**
     * Optimized rendering of BinaryPredicate using unification if the operator is 'EQUAL'.
     *
     * Example:
     * "get(State_0, 'x', Expr_0), Expr_0 = 2" will be rewritten to "get(State_0, 'x', 2)"
     *
     * @param node The node to render
     * @return The render result or null, if optimization is not applicable
     */
    fun renderOptimized(node: BinaryPredicate): RenderResult? {
        if ((node.operator == BinaryPredicateOperator.EQUAL || node.operator == BinaryPredicateOperator.NOT_EQUAL)  &&
            (node.right is IdentifierExpression
                    || node.right is SetEntry
                    || node.right is CollectionEntry
                    || node.right is ValueExpression)
        ) {
            if (node.left !is IdentifierExpression) return null

            var before = ""
            val rhs = when (node.right) {
                is IdentifierExpression -> {
                    if (environment.isConstant((node.right as IdentifierExpression))) {
                        evaluated[node.right] ?: run {
                            before = node.right.render().rendered
                            before += EXPRESSION_SEPARATOR
                            evaluated[node.right]
                        }
                    } else {
                        "'${(node.right as IdentifierExpression).name}'"
                    }
                    evaluated[node.right] ?: "'${(node.right as IdentifierExpression).name}'"
                }
                is SetEntry -> "'${(node.right as SetEntry).name}'"
                is CollectionEntry -> "'${(node.right as CollectionEntry).name}'"
                is ValueExpression -> (node.right as ValueExpression).rendered()
                else -> "" // when is exhaustive
            }

            val map = mapOf(
                "lhs" to (node.left as IdentifierExpression).name,
                "rhs" to rhs,
                "stateCount" to environment.stateCount,
                "negate" to (node.operator == BinaryPredicateOperator.NOT_EQUAL)
            )
            val rendered = environment.renderTemplate("optimizedBinaryPredicateEqual", map)
            return RenderResult("$before$rendered")
        }
        return null
    }

    // TODO: optimize while loops

    private fun ValueExpression.rendered(): String {
        return when (type) {
            is TypeBoolean -> render().rendered
            is TypeInteger -> render().rendered
            else -> ""
        }
    }

    fun loadIfEvaluated(node: MPPCGNode): RenderResult? {
        if (evaluated.containsKey(node)) {
            return RenderResult(evaluated[node] ?: "")
        }
        return null
    }
}