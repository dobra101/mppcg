package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.expression.Expression

data class AssignSubstitution(
    val left: Expression,
    val right: Expression
) : Substitution("assignSubstitution")
