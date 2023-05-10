package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

// TODO: split into multiple files
data class CallFunctionExpression(
    val expression: Expression,
    val parameters: List<Expression>,
    var operator: CallFunctionOperator = CallFunctionOperator.GET
) : Expression("callFunction", null)

enum class CallFunctionOperator : BMethod {
    GET,
    SET
}

data class UnaryFunctionExpression(
    val expression: Expression,
    val operator: UnaryFunctionOperator
) : Expression("unaryFunctionExpression")

data class BinaryFunctionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryFunctionOperator
) : Expression("binaryFunctionExpression")

enum class UnaryFunctionOperator : BMethod {
    DOMAIN,
    RANGE,
    REVERSE
}


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