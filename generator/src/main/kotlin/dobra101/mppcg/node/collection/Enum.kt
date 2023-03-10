package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeCollection

data class EnumCollectionNode(
    override var name: String,
    override val elements: List<EnumEntry>
) : CollectionNode(name, elements, CollectionType.Enum, "enumCollectionExpression") {
    override fun copy(): CollectionNode {
        return EnumCollectionNode(name, elements)
    }
}

data class EnumEntry(
    override var name: String,
    val enum: String
) : CollectionEntry(name, enum, type = TypeCollection(CollectionType.Enum, enum), "enumEntryExpression")
