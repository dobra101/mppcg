package dobra101.mppcg.node.b

import dobra101.mppcg.node.predicate.Predicate
import dobra101.mppcg.node.substitution.Substitution

data class Precondition(
    val substitution: Substitution?,
    val predicate: Predicate
) : Substitution("precondition")
