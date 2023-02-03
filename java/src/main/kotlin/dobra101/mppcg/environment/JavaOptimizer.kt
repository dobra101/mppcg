package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.expression.*
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
            val binaryExpr = node.rhs[0] as BinaryExpression

            // TODO: when more than one identifier?
            // e.g. a = a + 1 or a = 1 + a
            return assignSelf(binaryExpr.left, binaryExpr.right, binaryExpr.operator, node.lhs[0])
                ?: assignSelf(binaryExpr.right, binaryExpr.left, binaryExpr.operator, node.lhs[0])
        }
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
        if (target is IdentifierExpression && left == target) {
            val map = if (canBeFurtherOptimized(right, operator)) {
                mapOf(
                    "identifier" to target.name,
                    "operator" to if (operator == BinaryExpressionOperator.ADD) "++" else "--"
                )
            } else {
                mapOf(
                    "identifier" to target.name,
                    "operator" to environment.operator2String(operator),
                    "rhs" to right.render()
                )
            }
            return RenderResult(environment.renderTemplate("optimizedAssignSubstitution", map))
        }
        return null
    }
}