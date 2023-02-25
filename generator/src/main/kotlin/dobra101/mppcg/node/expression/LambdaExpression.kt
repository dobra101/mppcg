package dobra101.mppcg.node.expression

import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.predicate.Predicate

// TODO: fix type
data class LambdaExpression(
    val identifiers: List<Expression> = emptyList(),
    val predicate: Predicate,
    val expression: Expression
): Expression(TypeInteger(), "lambdaExpression")