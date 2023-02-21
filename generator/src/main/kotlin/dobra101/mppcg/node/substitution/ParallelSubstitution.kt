package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.expression.IdentifierExpression

data class ParallelSubstitution(
    val substitutions: List<Substitution>
) : Substitution("parallelSubstitution") {
    val needTempVar: Set<IdentifierExpression>
        get() {
            val assignments = substitutions.filterIsInstance<AssignSubstitution>()

            val identifierOnLhs = assignments.flatMap {
                it.lhs.filterIsInstance<IdentifierExpression>()
            }.toSet()

            // TODO: stimmt nicht für z.b. x := y + 1
            val identifierOnRhs = assignments.flatMap {
                it.rhs.filterIsInstance<IdentifierExpression>()
            }.toSet()

            return identifierOnLhs.intersect(identifierOnRhs)
        }
}
