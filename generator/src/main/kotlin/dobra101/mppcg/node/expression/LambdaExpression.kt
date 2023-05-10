package dobra101.mppcg.node.expression

import dobra101.mppcg.node.predicate.Predicate

data class LambdaExpression(
    val identifiers: List<Expression> = emptyList(),
    val predicate: Predicate,
    val expression: Expression
): Expression("lambdaExpression")