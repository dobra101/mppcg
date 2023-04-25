package dobra101.mppcg.node.expression

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeCouple
import dobra101.mppcg.node.predicate.Predicate

data class LambdaExpression(
    val identifiers: List<Expression> = emptyList(),
    val predicate: Predicate,
    val expression: Expression
): Expression(getType(identifiers, expression), "lambdaExpression")

private fun getType(identifiers: List<Expression>, expression: Expression): Type {
    return TypeCouple(identifiers[0].type, expression.type)
}