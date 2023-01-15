package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.expression.Expression

data class BinaryCollectionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryCollectionOperator
) : Expression(TypeInteger())

enum class BinaryCollectionOperator {
    INTERSECTION,
    SUBTRACTION,
    UNION
}