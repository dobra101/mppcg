package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.expression.Expression

// TODO: fix expression types
data class IntersectionCollectionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())

data class SubtractionCollectionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())


data class UnionCollectionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())