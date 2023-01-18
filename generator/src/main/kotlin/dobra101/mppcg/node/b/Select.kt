package dobra101.mppcg.node.b

import dobra101.mppcg.node.predicate.Predicate
import dobra101.mppcg.node.substitution.Substitution

data class Select(
    val condition: Predicate,
    val then: Substitution?, // null if skip
    val whenSubstitution: List<Substitution> = emptyList(),
    val elseSubstitution: Substitution? = null
) : Substitution()
