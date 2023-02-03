package dobra101.mppcg.node.expression

import dobra101.mppcg.node.Type

data class IdentifierExpression(
    val name: String,
    override var type: Type? = null
) : Expression(type, "identifierExpression")