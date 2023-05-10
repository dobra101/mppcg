package dobra101.mppcg.node.b

import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.Type

data class ConcreteIdentifierExpression(
    private val _name: String,
    val value: Expression,
    override var type: Type?
) : Expression("concreteIdentifier", type), NamedNode {
    override var name = "c_$_name"

    override fun equals(other: Any?): Boolean {
        if (other !is IdentifierExpression) return false
        return other.name == name
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }
}