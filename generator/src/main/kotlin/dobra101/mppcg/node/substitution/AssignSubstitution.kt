package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.expression.Expression

data class AssignSubstitution(
    val lhs: Expression,
    val rhs: Expression
) : Substitution("assignSubstitution")
