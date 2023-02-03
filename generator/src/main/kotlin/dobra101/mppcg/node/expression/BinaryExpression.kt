package dobra101.mppcg.node.expression

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeReal
import dobra101.mppcg.node.TypeInteger

data class BinaryExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryExpressionOperator
) : Expression(getType(left, right), "binaryExpression")

enum class BinaryExpressionOperator {
    ADD,
    MINUS,
    MULT,
    DIV
}

// TODO: replace by type inference
private fun getType(left: Expression, right: Expression): Type {
    if (left.type is TypeReal || right.type is TypeReal) return TypeReal()
    return TypeInteger()
}