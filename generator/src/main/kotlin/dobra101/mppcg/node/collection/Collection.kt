package dobra101.mppcg.node.collection

import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.expression.Expression

abstract class CollectionNode(
    override var name: String,
    open val elements: List<CollectionEntry>,
    override val templateName: String,
    var isParameter: Boolean = false
) : Expression(templateName), NamedNode {
    abstract fun copy(): CollectionNode
}

abstract class CollectionEntry(
    override var name: String,
    open val collection: String,
    override val templateName: String
) : Expression(templateName), NamedNode