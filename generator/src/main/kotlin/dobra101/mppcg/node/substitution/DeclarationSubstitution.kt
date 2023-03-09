package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.Type

data class DeclarationSubstitution(
    val type: Type,
    val assignment: AssignSubstitution
): Substitution("declarationSubstitution")
