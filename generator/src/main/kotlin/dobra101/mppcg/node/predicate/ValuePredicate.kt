package dobra101.mppcg.node.predicate

data class ValuePredicate(
    val value: String = ""
) : Predicate("valuePredicate")