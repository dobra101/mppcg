package dobra101.mppcg.node.collection

import dobra101.mppcg.node.expression.Expression

data class AnonymousCollectionNode(
    val elements: List<Expression> = listOf()
) : Expression("anonymousSetCollectionExpression")