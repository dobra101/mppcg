package dobra101.mppcg.node.b

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.IdentifierExpression

data class ConcreteIdentifierExpression(
    val name: String,
    val value: Expression,
    override var type: Type?
): Expression(type, "concreteIdentifier") {
    override fun equals(other: Any?): Boolean {
        if (other !is IdentifierExpression) return false
        return other.name == name
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }
}