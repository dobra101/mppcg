package dobra101.mppcg.node.b

import dobra101.mppcg.node.predicate.Predicate

data class Invariant(
    val predicate: Predicate
) : Predicate()