package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeAnonymousCollection
import dobra101.mppcg.node.TypeString
import dobra101.mppcg.node.expression.Expression

abstract class AnonymousCollectionNode(
    open val elements: List<AnonymousCollectionEntry>,
    private val collectionType: AnonymousCollectionType
) : Expression(type = TypeAnonymousCollection(collectionType))

abstract class AnonymousCollectionEntry(open val name: String) :
    Expression(type = TypeString()) // TODO: fix type

enum class AnonymousCollectionType {
    List,
    Set
}