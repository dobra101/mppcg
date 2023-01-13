package dobra101.mppcg.node.b

import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.expression.Expression

// TODO: fix expression types
data class CallFunctionExpression(
    val expression: Expression,
    val parameters: List<Expression>
) : Expression(TypeInteger())

data class DomainFunctionExpression(
    val expression: Expression
) : Expression(TypeInteger())

data class DomainRestrictionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())

data class DomainSubtractionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())

data class ImageFunctionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())

data class RangeFunctionExpression(
    val expression: Expression
) : Expression(TypeInteger())

data class RangeRestrictionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())

data class RangeSubtractionExpression(
    val left: Expression,
    val right: Expression
) : Expression(TypeInteger())

data class ReverseFunctionExpression(
    val expression: Expression
) : Expression(TypeInteger())