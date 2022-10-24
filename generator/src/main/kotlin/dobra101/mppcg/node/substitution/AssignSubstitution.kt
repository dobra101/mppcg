package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.expression.Expression

// TODO: when more than one entry in list?
data class AssignSubstitution(
    val lhs: List<Expression> = emptyList(),
    val rhs: List<Expression> = emptyList()
) : Substitution()
