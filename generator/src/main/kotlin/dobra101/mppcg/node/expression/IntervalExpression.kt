package dobra101.mppcg.node.expression

data class IntervalExpression(
    val left: Expression,
    val right: Expression
) : Expression("intervalExpression")