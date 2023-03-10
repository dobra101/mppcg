package dobra101.mppcg.node.collection

import dobra101.mppcg.node.expression.Expression

data class SetCollectionNode(
    override var name: String,
    override val elements: List<SetEntry>
) : CollectionNode(name, elements, CollectionType.Set, "setCollectionExpression") {
    override fun copy(): CollectionNode {
        return SetCollectionNode(name, elements)
    }
}

data class SetEntry(
    override var name: String,
    val set: String
) : CollectionEntry(name, set, templateName = "setEntryExpression")

data class AnonymousSetCollectionNode(
    override val elements: List<Expression> = emptyList(),
) : AnonymousCollectionNode(elements, CollectionType.Set)
