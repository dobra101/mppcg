package dobra101.mppcg.node.collection

data class SetCollectionNode(
    override var name: String,
    override val elements: List<SetEntry>
) : CollectionNode(name, elements, "setCollectionExpression") {
    override fun copy(): CollectionNode {
        return SetCollectionNode(name, elements)
    }
}

data class SetEntry(
    override var name: String,
    val set: String
) : CollectionEntry(name, set, templateName = "setEntryExpression")
