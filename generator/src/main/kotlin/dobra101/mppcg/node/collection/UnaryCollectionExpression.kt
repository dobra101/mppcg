package dobra101.mppcg.node.collection

import dobra101.mppcg.node.CustomMethodOperator
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeNat
import dobra101.mppcg.node.TypeSet
import dobra101.mppcg.node.expression.Expression

data class UnaryCollectionExpression(
    val collection: Expression,
    val operator: UnaryCollectionOperator
) : Expression(getType(collection, operator), "unaryCollectionExpression")

// TODO: not in class
private fun getType(expr: Expression, operator: UnaryCollectionOperator): Type? {
    if (operator == UnaryCollectionOperator.CARD) return TypeNat()
    // TODO: fix
    return when (operator) {
        UnaryCollectionOperator.MAX, UnaryCollectionOperator.MIN -> {
            if (expr.type is TypeSet) (expr.type as TypeSet).type
            else expr.type
        }

        UnaryCollectionOperator.POW, UnaryCollectionOperator.POW1 -> {
            expr.type
        }

        else -> expr.type
    }
}

enum class UnaryCollectionOperator : CustomMethodOperator {
    CARD,
    MAX,
    MIN,
    POW,
    POW1
}