package dobra101.mppcg.node.collection

data class EnumCollectionNode(
    override var name: String,
    override val elements: List<EnumEntry>,
) : CollectionNode(name, elements, CollectionType.Enum, "enumCollectionExpression") {
    override fun copy(): CollectionNode {
        return EnumCollectionNode(name, elements)
    }
}

// TODO: enum: String required?
data class EnumEntry(
    override var name: String,
    val enum: String
) : CollectionEntry(name, enum, "enumEntryExpression")
