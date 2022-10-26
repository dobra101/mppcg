package dobra101.mppcg.node.collection

data class EnumCollectionNode(
    override val name: String,
    override val elements: List<EnumEntry>
) : CollectionNode(name, elements, CollectionType.Enum)

data class EnumEntry(override val name: String, val enum: String) : CollectionEntry(name, enum)
