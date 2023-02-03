package dobra101.mppcg.node.predicate

data class LogicPredicate(
    val left: Predicate,
    val right: Predicate,
    val operator: LogicPredicateOperator
) : Predicate("logicPredicate")

enum class LogicPredicateOperator {
    AND,
    OR,
    IMPLIES
}