package dobra101.mppcg.node.b

import dobra101.mppcg.node.*
import dobra101.mppcg.node.collection.CollectionType
import dobra101.mppcg.node.expression.Expression

// TODO: fix expression types
data class CallFunctionExpression(
    val expression: Expression,
    val parameters: List<Expression>
) : Expression(null, "callFunction") {
    companion object CallFunctionOperator : BMethod
}

data class UnaryFunctionExpression(
    val expression: Expression,
    val operator: UnaryFunctionOperator
) : Expression(getType(expression, operator), "unaryFunctionExpression")

data class BinaryFunctionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryFunctionOperator
) : Expression(getType(left, right, operator), "binaryFunctionExpression")

enum class UnaryFunctionOperator : BMethod {
    DOMAIN,
    RANGE,
    REVERSE
}

private fun getType(left: Expression, right: Expression, operator: BinaryFunctionOperator): Type? {
    return when (operator) {
        BinaryFunctionOperator.DOMAIN_RESTRICTION,
        BinaryFunctionOperator.DOMAIN_SUBTRACTION -> right.type

        BinaryFunctionOperator.IMAGE -> ((right as Function).type as TypeFunction).to

        BinaryFunctionOperator.OVERWRITE,
        BinaryFunctionOperator.RANGE_RESTRICTION,
        BinaryFunctionOperator.RANGE_SUBTRACTION -> left.type

        BinaryFunctionOperator.FORWARD_COMPOSITION -> {
            left as Function
            right as Function
            TypeFunction(left.functionType, (left.type as TypeFunction).from, (right.type as TypeFunction).to)
        }
    }
}

enum class BinaryFunctionOperator : BMethod {
    DOMAIN_RESTRICTION,
    DOMAIN_SUBTRACTION,
    IMAGE,
    OVERWRITE,
    RANGE_RESTRICTION,
    RANGE_SUBTRACTION,
    FORWARD_COMPOSITION
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