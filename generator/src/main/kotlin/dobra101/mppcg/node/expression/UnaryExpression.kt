package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeBoolean
import dobra101.mppcg.node.b.BMethod

data class UnaryExpression(
    val value: MPPCGNode,
    val operator: UnaryExpressionOperator
) : Expression(getType(value, operator), "unaryExpression")

// TODO: refactor
enum class UnaryExpressionOperator: BMethod {
    CONVERT_BOOLEAN,
    // TODO: only for B
    PRED,
    SUCC,
    MINUS
}

private fun getType(value: MPPCGNode, operator: UnaryExpressionOperator): Type? {
    if (value is ValueExpression) return value.type
    if (operator == UnaryExpressionOperator.CONVERT_BOOLEAN) return TypeBoolean()
    return (value as Expression).type
}