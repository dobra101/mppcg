package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeAnonymousCollection
import dobra101.mppcg.node.expression.Expression

abstract class AnonymousCollectionNode(
    open val elements: List<Expression>,
    private val collectionType: CollectionType
) : Expression(type = TypeAnonymousCollection(collectionType), "anonymousSetCollectionExpression")