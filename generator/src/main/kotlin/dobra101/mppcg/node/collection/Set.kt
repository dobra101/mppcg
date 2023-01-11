package dobra101.mppcg.node.collection

data class SetCollectionNode(
    override val name: String,
    override val elements: List<SetEntry>
) : CollectionNode(name, elements, CollectionType.Set)

data class SetEntry(override val name: String, val set: String) : CollectionEntry(name, set)

data class AnonymousSetCollectionNode(
    override val elements: List<AnonymousSetEntry> = emptyList(),
) : AnonymousCollectionNode(elements, AnonymousCollectionType.Set)

data class AnonymousSetEntry(override val name: String): AnonymousCollectionEntry(name) // TODO: fix type
