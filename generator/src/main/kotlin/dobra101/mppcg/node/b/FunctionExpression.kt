package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression

data class CallFunctionExpression(
    val expression: Expression,
    val parameters: List<Expression>,
    var operator: CallFunctionOperator = CallFunctionOperator.GET
) : Expression("callFunction")

enum class CallFunctionOperator : BMethod {
    GET,
    SET
}