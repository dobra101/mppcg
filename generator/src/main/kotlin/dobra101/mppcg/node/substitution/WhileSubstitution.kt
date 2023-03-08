package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.predicate.Predicate

data class WhileSubstitution(val condition: Predicate, val body: Substitution) : Substitution("whileSubstitution")