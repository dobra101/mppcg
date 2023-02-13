package dobra101.mppcg.node.predicate

import dobra101.mppcg.node.CustomMethodOperator
import dobra101.mppcg.node.expression.Expression

data class BinaryPredicate(
    val left: Expression,
    val right: Expression,
    val operator: BinaryPredicateOperator
) : Predicate("binaryPredicate")

enum class BinaryPredicateOperator: CustomMethodOperator {
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    EQUAL,
    NOT_EQUAL,
    MEMBER,
    NOT_MEMBER,
    SUBSET
}
