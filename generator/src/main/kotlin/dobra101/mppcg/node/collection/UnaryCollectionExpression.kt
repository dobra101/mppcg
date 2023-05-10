package dobra101.mppcg.node.collection

import dobra101.mppcg.node.CustomMethodOperator
import dobra101.mppcg.node.expression.Expression

data class UnaryCollectionExpression(
    val collection: Expression,
    val operator: UnaryCollectionOperator
) : Expression("unaryCollectionExpression")

enum class UnaryCollectionOperator : CustomMethodOperator {
    CARD,
    MAX,
    MIN,
    POW,
    POW1
}