package dobra101.mppcg.node.predicate

data class BinaryLogicPredicate(
    val left: Predicate,
    val right: Predicate,
    val operator: LogicPredicateOperator
) : Predicate("binaryLogicPredicate")

data class UnaryLogicPredicate(
    val predicate: Predicate,
    val operator: LogicPredicateOperator
) : Predicate("unaryLogicPredicate")

enum class LogicPredicateOperator {
    AND,
    OR,
    IMPLIES,
    EQUIVALENCE,
    NOT
}