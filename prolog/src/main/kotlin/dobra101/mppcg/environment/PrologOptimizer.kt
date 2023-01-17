package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.b.Precondition
import dobra101.mppcg.node.collection.CollectionEntry
import dobra101.mppcg.node.collection.SetEntry
import dobra101.mppcg.node.expression.IdentifierExpression
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
        if (node.operator == BinaryPredicateOperator.EQUAL &&
            (node.right is IdentifierExpression || node.right is SetEntry || node.right is CollectionEntry)
        ) {
            if (node.left !is IdentifierExpression) return null

            val rhs = when (node.right) {
                is IdentifierExpression -> (node.right as IdentifierExpression).name
                is SetEntry -> (node.right as SetEntry).name
                is CollectionEntry -> (node.right as CollectionEntry).name
                else -> "" // when is exhaustive
            }

            val map = mapOf(
                "lhs" to (node.left as IdentifierExpression).name,
                "rhs" to rhs,
                "stateCount" to environment.stateCount
            )
            return RenderResult(environment.stRender("optimizedBinaryPredicateEqual", map))
        }
        return null
    }

    fun renderOptimized(node: Precondition): RenderResult? {
        evaluated = hashMapOf()

        val predicateRendered = node.predicate.render()
        val substitutionRendered = node.substitution.render()

        val map = mapOf(
            "predicate" to predicateRendered.rendered,
            "substitution" to substitutionRendered.rendered
        )

        return RenderResult(environment.stRender("precondition", map))
    }

    fun loadIfEvaluated(node: MPPCGNode): RenderResult? {
        if (evaluated.containsKey(node)) {
            return RenderResult(evaluated[node] ?: "")
        }
        return null
    }
}