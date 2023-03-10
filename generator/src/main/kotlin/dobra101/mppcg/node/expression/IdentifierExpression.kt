package dobra101.mppcg.node.expression

import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.Type

data class IdentifierExpression(
    override var name: String,
    override var type: Type? = null
) : Expression(type, "identifierExpression"), NamedNode {
    override fun equals(other: Any?): Boolean {
        if (other !is IdentifierExpression) return false
        return other.name == name
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }
}