package dobra101.mppcg.node.collection

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.expression.Expression

data class AnonymousCollectionNode(
    val elements: List<Expression> = listOf(),
    var collectionType: Type? = null
) : Expression("anonymousSetCollectionExpression")