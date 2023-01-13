package dobra101.mppcg.node.collection

import dobra101.mppcg.node.expression.Expression

data class SetCollectionNode(
    override val name: String,
    override val elements: List<SetEntry>
) : CollectionNode(name, elements, CollectionType.Set)

data class SetEntry(override val name: String, val set: String) : CollectionEntry(name, set)

data class AnonymousSetCollectionNode(
    override val elements: List<Expression> = emptyList(),
) : AnonymousCollectionNode(elements, AnonymousCollectionType.Set)
