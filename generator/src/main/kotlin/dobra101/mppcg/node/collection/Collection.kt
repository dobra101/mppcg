package dobra101.mppcg.node.collection

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.TypeString
import dobra101.mppcg.node.expression.Expression

abstract class CollectionNode(open val name: String, open val elements: List<CollectionEntry>, val collectionType: CollectionType) : MPPCGNode

abstract class CollectionEntry(open val name: String, open val collection: String) : Expression(type = TypeString()) // TODO: fix type

enum class CollectionType {
    List,
    Set,
    Enum
}