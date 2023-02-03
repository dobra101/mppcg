package dobra101.mppcg.node.substitution

data class ParallelSubstitution(
    val substitutions: List<Substitution>
) : Substitution("parallelSubstitution")
