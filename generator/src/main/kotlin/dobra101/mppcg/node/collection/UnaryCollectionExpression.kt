package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.expression.Expression

// TODO: fix type
data class UnaryCollectionExpression(
    val collection: Expression,
    val operator: UnaryCollectionOperator
) : Expression(TypeInteger(), "unaryCollectionExpression")

enum class UnaryCollectionOperator {
    CARD,
    MIN,
    MAX,
    POW,
    POW1
}