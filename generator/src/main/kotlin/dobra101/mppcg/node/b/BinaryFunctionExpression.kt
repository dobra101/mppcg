package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

data class BinaryFunctionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryFunctionOperator
) : Expression("binaryFunctionExpression")


enum class BinaryFunctionOperator : BMethod {
    DOMAIN_RESTRICTION,
    DOMAIN_SUBTRACTION,
    IMAGE,
    OVERWRITE,
    RANGE_RESTRICTION,
    RANGE_SUBTRACTION,
    FORWARD_COMPOSITION,
    ITERATE
}