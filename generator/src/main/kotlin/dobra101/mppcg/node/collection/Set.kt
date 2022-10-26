package dobra101.mppcg.node.collection

data class SetCollectionNode(
    override val name: String,
    override val elements: List<SetEntry>
) : CollectionNode(name, elements, CollectionType.Set)

data class SetEntry(override val name: String, val set: String) : CollectionEntry(name, set)