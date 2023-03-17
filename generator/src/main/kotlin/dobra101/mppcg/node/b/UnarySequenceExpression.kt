package dobra101.mppcg.node.b

import dobra101.mppcg.node.CustomMethodOperator
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeSequence
import dobra101.mppcg.node.expression.Expression

// TODO: fix type = sequence type
data class UnarySequenceExpression(
    val sequence: Expression,
    val operator: UnarySequenceExpressionOperator
) : Expression(typeByOperator(sequence, operator), "unarySequenceExpression")

// TODO: fix type
data class BinarySequenceExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinarySequenceExpressionOperator
) : Expression(left.type, "binarySequenceExpression")

// TODO: refactor
private fun typeByOperator(sequence: Expression, operator: UnarySequenceExpressionOperator): Type? {
    return when (operator) {
        UnarySequenceExpressionOperator.CONCAT,
        UnarySequenceExpressionOperator.REVERSE,
        UnarySequenceExpressionOperator.FRONT,
        UnarySequenceExpressionOperator.TAIL -> sequence.type

        UnarySequenceExpressionOperator.FIRST,
        UnarySequenceExpressionOperator.LAST -> (sequence.type as? TypeSequence)?.type
    }
}

enum class UnarySequenceExpressionOperator : CustomMethodOperator {
    FRONT,
    TAIL,
    FIRST,
    LAST,
    REVERSE,
    CONCAT
}

enum class BinarySequenceExpressionOperator : CustomMethodOperator {
    RESTRICT_FRONT,
    RESTRICT_TAIL,
    APPEND,
    PREPEND,
    CONCAT
}
