package dobra101.mppcg.node.collection

import dobra101.mppcg.node.*
import dobra101.mppcg.node.expression.Expression

data class BinaryCollectionExpression(
    val left: Expression,
    val right: Expression,
    val operator: BinaryCollectionOperator
) : Expression(getType(left, right, operator), "binaryCollectionExpression")

enum class BinaryCollectionOperator: CustomMethodOperator {
    INTERSECTION,
    SUBTRACTION,
    UNION,
    CONCAT,
    PRJ1,
    PRJ2
}

// TODO: replace by type inference
private fun getType(left: Expression, right: Expression, operator: BinaryCollectionOperator): Type? {
    if (left.type == null) return right.type
    if (right.type == null) return left.type

    // number type TODO: other has to be number
    if (operator == BinaryCollectionOperator.CONCAT) {
        return TypeSequence((left.type as TypeCouple).to)
    }

    if (left.type is TypeReal || right.type is TypeReal) return TypeReal()
    if (left.type is TypeSequence && right.type is TypeFunction) return left.type
    if (left.type is TypeFunction && right.type is TypeSequence) return right.type

    if (left.type != right.type) {
        if (right.type is TypeAnonymousCollection) {
            return left.type
        }

        if (left.type is TypeAnonymousCollection) {
            println((left.type as TypeAnonymousCollection).type)
        }
        if (right.type is TypeCollection) {
            println(((right.type as TypeCollection).type))
        }
        println("Left (${left.type}): $left")
        println("Right (${right.type}): $right")
        throw InvalidTypeException("Types ${left.type} and ${right.type} do not match.")
    }
    return left.type
}