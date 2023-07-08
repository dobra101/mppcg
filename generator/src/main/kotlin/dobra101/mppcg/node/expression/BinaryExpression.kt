package dobra101.mppcg.node.expression

data class BinaryExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryExpressionOperator
) : Expression("binaryExpression")

enum class BinaryExpressionOperator {
    ADD,
    MINUS,
    MULT,
    DIV,
    MOD,
    POW,
    PARALLEL_PRODUCT
}