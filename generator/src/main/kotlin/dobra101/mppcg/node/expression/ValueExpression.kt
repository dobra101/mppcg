package dobra101.mppcg.node.expression

import dobra101.mppcg.node.Type

data class ValueExpression(
    val value: String = "",
    override var type: Type? = null
) : Expression("valueExpression", type)
