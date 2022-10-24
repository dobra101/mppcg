package dobra101.mppcg.node.collection

data class SetCollectionNode(val name: String, val element: List<SetEntry>) : CollectionNode(CollectionType.Set)

data class SetEntry(val name: String, val set: String)