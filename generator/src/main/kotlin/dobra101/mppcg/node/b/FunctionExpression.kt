package dobra101.mppcg.node.b

import dobra101.mppcg.node.*
import dobra101.mppcg.node.collection.CollectionType
import dobra101.mppcg.node.expression.Expression

// TODO: fix expression types
data class CallFunctionExpression(
    val expression: Expression,
    val parameters: List<Expression>
) : Expression(expression.type, "callFunction")

data class UnaryFunctionExpression(
    val expression: Expression,
    val operator: UnaryFunctionOperator
) : Expression(getType(expression, operator), "unaryFunctionExpression")

data class BinaryFunctionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryFunctionOperator
) : Expression(left.type, "binaryFunctionExpression")

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

// TODO: duplicate
private fun getType(expression: Expression, operator: UnaryFunctionOperator): Type? {
    // TODO: fix types
    return when (operator) {
        UnaryFunctionOperator.DOMAIN -> TypeCollection(CollectionType.Set)
        UnaryFunctionOperator.RANGE -> TypeCollection(CollectionType.Set)
        UnaryFunctionOperator.REVERSE -> expression.type
    }
}