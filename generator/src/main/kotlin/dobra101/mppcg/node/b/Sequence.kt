package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

data class Sequence(
    val elements: List<Expression> = emptyList()
) : Expression("sequence")