package dobra101.mppcg.node.b

import dobra101.mppcg.node.substitution.Substitution

data class Initialization(
    val substitutions: List<Substitution> = emptyList()
) : Substitution()
