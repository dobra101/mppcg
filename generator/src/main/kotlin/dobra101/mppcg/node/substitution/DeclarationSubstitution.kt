package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.Type

data class DeclarationSubstitution(
    val assignment: AssignSubstitution,
    val type: Type? = null // TODO: type needed? -> only for java
): Substitution("declarationSubstitution")
