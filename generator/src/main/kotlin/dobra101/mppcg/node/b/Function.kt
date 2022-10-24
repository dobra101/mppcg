package dobra101.mppcg.node.b

import dobra101.mppcg.node.TypeFunction
import dobra101.mppcg.node.expression.Expression

data class Function(
    val left: Expression,
    val right: Expression,
    override val type: TypeFunction,
    val mapType: FunctionMapType
): Expression(type)

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