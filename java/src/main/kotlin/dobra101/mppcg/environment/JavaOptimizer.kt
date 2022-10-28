package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.expression.BinaryExpression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.substitution.AssignSubstitution

class JavaOptimizer(private val environment: JavaOutputEnvironment) {

    /**
     * Optimized rendering of AssignSubstitution.
     *
     * Example:
     * "a = a + 1" will be rewritten to "a += 1"
     *
     * @param node The node to render
     * @return The render result or null, if optimization is not applicable
     */
    fun renderOptimized(node: AssignSubstitution): RenderResult? {
        if (node.rhs.size == 1 && node.rhs[0] is BinaryExpression) {
            // e.g. a = a + 1
            if ((node.rhs[0] as BinaryExpression).left == node.lhs[0]) {
                val identifier = (node.lhs[0] as IdentifierExpression).name // TODO: when more than one identifier?
                val map = mapOf(
                    "identifier" to identifier,
                    "operator" to environment.operator2String((node.rhs[0] as BinaryExpression).operator),
                    "rhs" to (node.rhs[0] as BinaryExpression).right.render()
                )
                return RenderResult(environment.stRender("optimizedAssignSubstitution", map))
            }

            // e.g. a = 1 + a
            if ((node.rhs[0] as BinaryExpression).right == node.lhs[0]) {
                val map = mapOf(
                    "identifier" to (node.lhs[0] as IdentifierExpression).name,
                    "operator" to environment.operator2String((node.rhs[0] as BinaryExpression).operator),
                    "rhs" to (node.rhs[0] as BinaryExpression).left.render()
                )
                return RenderResult(environment.stRender("optimizedAssignSubstitution", map))
            }
        }
        return null
    }
}