package dobra101.mppcg.node.collection

import dobra101.mppcg.node.TypeCollection

data class EnumCollectionNode(
    override val name: String,
    override val elements: List<EnumEntry>
) : CollectionNode(name, elements, CollectionType.Enum, "enumCollectionExpression")

data class EnumEntry(
    override val name: String,
    val enum: String
) : CollectionEntry(name, enum, type = TypeCollection(CollectionType.Enum, enum), "enumEntryExpression")
