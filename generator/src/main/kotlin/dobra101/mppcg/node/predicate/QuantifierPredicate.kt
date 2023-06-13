package dobra101.mppcg.node.predicate

import dobra101.mppcg.node.expression.IdentifierExpression

data class QuantifierPredicate(
    val identifier: List<IdentifierExpression>,
    val predicate: Predicate,
    val quantification: Predicate? = null,
    val type: QuantifierType
) : Predicate("quantifierPredicate")

enum class QuantifierType {
    FORALL,
    EXISTS
}