package dobra101.mppcg.node.expression

import dobra101.mppcg.adapter.sablecc.machineVisitor
import dobra101.mppcg.node.*

data class BinaryExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryExpressionOperator
) : Expression(getType(left, right, operator), "binaryExpression")

enum class BinaryExpressionOperator {
    ADD,
    MINUS,
    MULT,
    DIV,
    MOD
}

// TODO: replace by type inference
private fun getType(left: Expression, right: Expression, operator: BinaryExpressionOperator): Type? {
    if (operator == BinaryExpressionOperator.MULT && (left.type is TypeSet || right.type is TypeSet)) {
        return TypeCouple()
    }

    if (left.type == null) {
        machineVisitor.setTypeIfKnown(left, right.type)
        return right.type
    }
    if (right.type == null) {
        machineVisitor.setTypeIfKnown(right, left.type)
        return left.type
    }

    // number type
    if (left.type is TypeReal || right.type is TypeReal) return TypeReal()

    // TODO: refactor
    if (left.type != right.type) {
        if (left.type is TypeNatural) {
            return if (right.canBeNatural()) left.type else right.type
        }

        throw InvalidTypeException("Types ${left.type} and ${right.type} do not match.")
    }
    return left.type
}

private fun Expression.canBeNatural(): Boolean {
    if (type !is TypeInteger) return false
    if (this !is ValueExpression) return false
    if (value.toIntOrNull() == null) return false
    return value.toInt() >= 0
}