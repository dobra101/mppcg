package dobra101.mppcg.node.expression

import dobra101.mppcg.node.predicate.Predicate

data class GeneralSumOrProductExpression(
    val identifiers: List<IdentifierExpression>,
    val predicate: Predicate,
    val expression: Expression,
    val operation: SumOrProductOperation
) : Expression(expression.type, "generalSumOrProductExpression")

enum class SumOrProductOperation {
    SUM,
    PRODUCT
}