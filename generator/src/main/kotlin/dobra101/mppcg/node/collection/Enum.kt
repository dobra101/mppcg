package dobra101.mppcg.node.collection

data class EnumCollectionNode(
    override var name: String,
    override val elements: List<EnumEntry>,
) : CollectionNode(name, elements, "enumCollectionExpression") {
    override fun copy(): CollectionNode {
        return EnumCollectionNode(name, elements)
    }
}

data class EnumEntry(
    override var name: String,
    val enum: String
) : CollectionEntry(name, enum, "enumEntryExpression")
