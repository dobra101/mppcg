package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

data class UnarySequenceExpression(
    val sequence: Expression,
    val operator: UnarySequenceExpressionOperator
) : Expression("unarySequenceExpression")

data class BinarySequenceExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinarySequenceExpressionOperator
) : Expression("binarySequenceExpression")

enum class UnarySequenceExpressionOperator {
    FRONT,
    TAIL,
    FIRST,
    LAST,
    REVERSE
}

enum class BinarySequenceExpressionOperator {
    RESTRICT_FRONT,
    RESTRICT_TAIL,
    APPEND,
    PREPEND,
    CONCAT
}
