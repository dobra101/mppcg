package dobra101.mppcg.node.b

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeFunction
import dobra101.mppcg.node.expression.Expression

data class Function(
    val left: Expression,
    val right: Expression,
    val functionType: FunctionType,
    val mapType: FunctionMapType
) : Expression(deriveType(left, right, functionType), "function")

// TODO: not in class
private fun deriveType(left: Expression, right: Expression, functionType: FunctionType): Type {
    val leftType = if (left is InfiniteSet) left.setType else left.type
    val rightType = if (right is InfiniteSet) right.setType else right.type
    return TypeFunction(functionType, leftType, rightType)
}

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