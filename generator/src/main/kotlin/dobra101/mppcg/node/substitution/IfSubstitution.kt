package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.predicate.Predicate

data class IfSubstitution(
    val condition: Predicate,
    val then: Substitution,
    val elseIf: List<Substitution>,
    val elseSubstitution: Substitution
) : Substitution()
