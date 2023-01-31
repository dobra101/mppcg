package dobra101.mppcg.node.substitution

data class SequenceSubstitution(
    val substitutions: List<Substitution> = emptyList()
): Substitution()
