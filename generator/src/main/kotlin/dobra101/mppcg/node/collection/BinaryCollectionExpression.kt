package dobra101.mppcg.node.collection

import dobra101.mppcg.node.*
import dobra101.mppcg.node.expression.Expression

data class BinaryCollectionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryCollectionOperator
) : Expression(getType(left, right), "binaryCollectionExpression")

enum class BinaryCollectionOperator: CustomMethodOperator {
    INTERSECTION,
    SUBTRACTION,
    UNION
}

// TODO: replace by type inference
private fun getType(left: Expression, right: Expression): Type? {
    if (left.type == null) return right.type
    if (right.type == null) return left.type

    // number type
    if (left.type is TypeReal || right.type is TypeReal) return TypeReal()

    if (left.type != right.type) throw InvalidTypeException("Types ${left.type} and ${right.type} to not match.")
    return left.type
}