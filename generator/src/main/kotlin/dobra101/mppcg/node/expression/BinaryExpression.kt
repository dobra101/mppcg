package dobra101.mppcg.node.expression

import dobra101.mppcg.node.CustomMethodOperator

data class BinaryExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryExpressionOperator
) : Expression("binaryExpression")

enum class BinaryExpressionOperator: CustomMethodOperator {
    ADD,
    MINUS,
    MULT,
    DIV,
    MOD,
    POW,
    PARALLEL_PRODUCT
}