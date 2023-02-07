package dobra101.mppcg.node.collection

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeCollection
import dobra101.mppcg.node.expression.Expression

abstract class CollectionNode(
    open val name: String,
    open val elements: List<CollectionEntry>,
    private val collectionType: CollectionType,
    override val templateName: String,
    var isParameter: Boolean = false
) : Expression(type = TypeCollection(collectionType, name), templateName) {
    abstract fun copy(): CollectionNode
}

abstract class CollectionEntry(
    open val name: String,
    open val collection: String,
    override var type: Type? = null,
    override val templateName: String
) : Expression(type, templateName)

enum class CollectionType {
    List,
    Set,
    Enum
}