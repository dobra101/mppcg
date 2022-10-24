package dobra101.mppcg.node.b

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.predicate.Predicate

data class Transition(
    val name: String,
    val body: Predicate
) : MPPCGNode