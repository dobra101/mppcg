package dobra101.mppcg.node.expression

import dobra101.mppcg.adapter.sablecc.machineVisitor
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.FunctionType
import dobra101.mppcg.node.collection.AnonymousSetCollectionNode

data class BinaryExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryExpressionOperator
) : Expression(getType(left, right, operator), "binaryExpression")

enum class BinaryExpressionOperator: CustomMethodOperator {
    ADD,
    MINUS,
    MULT,
    DIV,
    MOD,
    POW,
    PARALLEL_PRODUCT
}

// TODO: replace by type inference
private fun getType(left: Expression, right: Expression, operator: BinaryExpressionOperator): Type? {
    if (operator == BinaryExpressionOperator.MULT && (left.type is TypeSet || right.type is TypeSet)) {
        val leftType = (left.type as? TypeSet)?.type ?: left.type
        val rightType = (right.type as? TypeSet)?.type ?: right.type
        return TypeCouple(leftType, rightType)
    }
    if (operator == BinaryExpressionOperator.MULT && (left.type is TypeCollection || right.type is TypeCollection)) {
        val leftType = if (left is AnonymousSetCollectionNode) left.elements[0].type else left.type
        val rightType = if (right is AnonymousSetCollectionNode) right.elements[0].type else right.type
        return TypeFunction(type = FunctionType.TOTAL, from = leftType, to = rightType)
    }

    if (operator == BinaryExpressionOperator.MINUS && right.type is TypeNat) {
        return left.type
    }

    if (operator == BinaryExpressionOperator.MINUS && right.type is TypeSet && left.type is TypeSet) {
        return left.type
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
        if (left.type is TypeNat) {
            return if (right.canBeNatural()) left.type else right.type
        }

        throw InvalidTypeException("Types ${left.type} and ${right.type} do not match ($operator).")
    }
    return left.type
}

private fun Expression.canBeNatural(): Boolean {
    if (type !is TypeInt) return false
    if (this !is ValueExpression) return false
    if (value.toIntOrNull() == null) return false
    return value.toInt() >= 0
}