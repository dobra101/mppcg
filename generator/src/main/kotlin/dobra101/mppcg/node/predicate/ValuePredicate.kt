package dobra101.mppcg.node.predicate

import dobra101.mppcg.node.Type

data class ValuePredicate(
    val value: String = "",
    var type: Type? = null
) : Predicate("valuePredicate")