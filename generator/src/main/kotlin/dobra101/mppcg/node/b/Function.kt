package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

data class Function(
    val left: Expression,
    val right: Expression,
    val functionType: FunctionType,
    val mapType: FunctionMapType
) : Expression("function")

enum class FunctionType {
    TOTAL,
    PARTIAL
}

enum class FunctionMapType {
    FUNCTION,
    INJECTION,
    SURJECTION,
    BIJECTION
}