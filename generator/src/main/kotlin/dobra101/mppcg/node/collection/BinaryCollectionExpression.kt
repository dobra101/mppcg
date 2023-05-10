package dobra101.mppcg.node.collection

import dobra101.mppcg.node.*
import dobra101.mppcg.node.expression.Expression

data class BinaryCollectionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryCollectionOperator
) : Expression("binaryCollectionExpression")

enum class BinaryCollectionOperator: CustomMethodOperator {
    INTERSECTION,
    SUBTRACTION,
    UNION,
    CONCAT,
    PRJ1,
    PRJ2
}