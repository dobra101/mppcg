package dobra101.mppcg.node.collection

data class EnumCollectionNode(val name: String, val elements: List<EnumEntry>) : CollectionNode(CollectionType.Enum)

data class EnumEntry(val name: String, val enum: String)
