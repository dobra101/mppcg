package dobra101.mppcg.node.expression

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeReal
import dobra101.mppcg.node.TypeInteger
import dobra101.mppcg.node.TypeInterval

data class IntervalExpression(
    val left: Expression,
    val right: Expression
) : Expression(getType(left, right), "intervalExpression")

// TODO: replace with type inference
private fun getType(left: Expression, right: Expression): Type {
    return if (left.type is TypeReal || right.type is TypeReal) {
        TypeReal()
    } else {
        TypeInteger()
    }
}