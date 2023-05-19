package dobra101.mppcg.node.substitution

data class DeclarationSubstitution(
    val assignment: AssignSubstitution,
): Substitution("declarationSubstitution")
