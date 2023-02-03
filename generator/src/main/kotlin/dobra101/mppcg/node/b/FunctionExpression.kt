package dobra101.mppcg.node.b

import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.expression.Expression

// TODO: fix expression types
data class CallFunctionExpression(
    val expression: Expression,
    val parameters: List<Expression>
) : Expression(TypeInteger(), "callFunction")

data class UnaryFunctionExpression(
    val expression: Expression,
    val operator: UnaryFunctionOperator
) : Expression(TypeInteger(), "unaryFunctionExpression")

data class BinaryFunctionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryFunctionOperator
) : Expression(TypeInteger(), "binaryFunctionExpression")

enum class UnaryFunctionOperator: BMethod {
    DOMAIN,
    RANGE,
    REVERSE
}

enum class BinaryFunctionOperator: BMethod {
    DOMAIN_RESTRICTION,
    DOMAIN_SUBTRACTION,
    IMAGE,
    OVERWRITE,
    RANGE_RESTRICTION,
    RANGE_SUBTRACTION,
}