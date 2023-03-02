package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeBoolean

class UnaryExpression(
    val value: MPPCGNode,
    val operator: UnaryExpressionOperator
) : Expression(getType(operator), "unaryExpression")

// TODO: refactor
enum class UnaryExpressionOperator {
    CONVERT_BOOLEAN
}

private fun getType(operator: UnaryExpressionOperator): Type {
    return TypeBoolean()
}