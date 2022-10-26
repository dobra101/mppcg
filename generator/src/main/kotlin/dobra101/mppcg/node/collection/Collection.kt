package dobra101.mppcg.node.collection

import dobra101.mppcg.node.MPPCGNode

abstract class CollectionNode(open val name: String, open val elements: List<CollectionEntry>, val collectionType: CollectionType) : MPPCGNode

abstract class CollectionEntry(open val name: String, open val collection: String)

enum class CollectionType {
    List,
    Set,
    Enum
}