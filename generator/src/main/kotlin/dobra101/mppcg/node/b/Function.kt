package dobra101.mppcg.node.b

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.expression.Expression

data class Function(
    val left: Expression,
    val right: Expression,
    override var type: Type?,
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