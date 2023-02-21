package dobra101.mppcg.node.expression

import dobra101.mppcg.node.*

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
private fun getType(left: Expression, right: Expression): Type? {
    if (left.type == null) return right.type
    if (right.type == null) return left.type

    // number type
    if (left.type is TypeReal || right.type is TypeReal) return TypeReal()

    if (left.type != right.type) {
        if (left.type is TypeNatural && right.canBeNatural()) {
            return left.type
        }

        throw InvalidTypeException("Types ${left.type} and ${right.type} to not match.")
    }
    return left.type
}

private fun Expression.canBeNatural(): Boolean {
    if (type !is TypeInteger) return false
    if (this !is ValueExpression) return false
    if (value.toIntOrNull() == null) return false
    return value.toInt() >= 0
}