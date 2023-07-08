package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.b.BMethod

data class UnaryExpression(
    val value: MPPCGNode,
    val operator: UnaryExpressionOperator
) : Expression(templateName = "unaryExpression")

enum class UnaryExpressionOperator: BMethod {
    CONVERT_BOOLEAN,
    PRED,
    SUCC,
    MINUS
}