package dobra101.mppcg.node.collection

import dobra101.mppcg.node.MPPCGNode

abstract class CollectionNode(val collectionType: CollectionType) : MPPCGNode

enum class CollectionType {
    List,
    Set,
    Enum
}