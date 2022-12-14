package dobra101.mppcg.node.expression

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeReal
import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.TypeInterval

data class IntervalExpression(
    val left: Expression,
    val right: Expression
) : Expression(getType(left, right))

// TODO: replace by type inference
private fun getType(left: Expression, right: Expression): Type {
    val type = if (left.type is TypeReal || right.type is TypeReal) {
        TypeReal()
    } else {
        TypeInteger()
    }
    return TypeInterval(type)
}