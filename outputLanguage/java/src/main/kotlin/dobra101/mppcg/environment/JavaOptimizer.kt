package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.TypeNumber
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.substitution.AssignSubstitution

// TODO: remove optimizer
// TODO: parts of optimizer are input language specific
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
        if (node.right is BinaryExpression) {
            val binaryExpr = node.right as BinaryExpression

            // e.g. a = a + 1 or a = 1 + a
            return assignSelf(binaryExpr.left, binaryExpr.right, binaryExpr.operator, node.left)
                ?: assignSelf(binaryExpr.right, binaryExpr.left, binaryExpr.operator, node.left)
        }
        return null
    }

    fun renderOptimized(node: Operation): RenderResult? {
        // TODO: optimize e.g. out = x; return out;
        return null
    }

    private fun canBeFurtherOptimized(expr: Expression, operator: BinaryExpressionOperator): Boolean {
        val op = operator == BinaryExpressionOperator.ADD || operator == BinaryExpressionOperator.MINUS
        return op && expr is ValueExpression && expr.value.toIntOrNull() == 1
    }

    private fun assignSelf(
        left: Expression,
        right: Expression,
        operator: BinaryExpressionOperator,
        target: Expression
    ): RenderResult? {
        if (target is IdentifierExpression && left == target && left.type is TypeNumber) {
            val map = if (canBeFurtherOptimized(right, operator)) {
                mapOf(
                    "identifier" to target.name,
                    "operator" to if (operator == BinaryExpressionOperator.ADD) "++" else "--"
                )
            } else {
                mapOf(
                    "identifier" to target.name,
                    "operator" to environment.render(operator),
                    "rhs" to right.render()
                )
            }
            return RenderResult(environment.renderTemplate("optimizedAssignSubstitution", map))
        }
        return null
    }
}