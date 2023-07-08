package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

data class UnaryFunctionExpression(
    val expression: Expression,
    val operator: UnaryFunctionOperator
) : Expression("unaryFunctionExpression")

enum class UnaryFunctionOperator : BMethod {
    DOMAIN,
    RANGE,
    REVERSE
}