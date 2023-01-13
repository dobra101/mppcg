package dobra101.mppcg.node.predicate

import dobra101.mppcg.node.expression.IdentifierExpression

class QuantifierPredicate(
    val identifier: IdentifierExpression,
    val predicate: Predicate,
    val type: QuantifierType
) : Predicate()

enum class QuantifierType {
    FORALL,
    EXISTS
}