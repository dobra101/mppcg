package dobra101.mppcg.node.b

import dobra101.mppcg.node.TypeCouple
import dobra101.mppcg.node.expression.Expression

data class Couple(
    val list: List<Expression>
): Expression(TypeCouple(), "couple")