package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.b.Machine
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate
import dobra101.mppcg.node.substitution.Substitution
import java.util.*

val machineVisitor = MachineVisitor() // TODO: doof hier in einer utils klasse -> benötigte felder static

/**
 * Converts the BParser node to an intermediate code representation.
 *
 * @return The intermediate code representation
 */
fun Start.convert(): Machine {
    this.apply(machineVisitor)
    return machineVisitor.result!!
}

/**
 * Converts a list of BParser nodes to a list of intermediate code representations.
 *
 * @return A list of intermediate code representations
 */
@JvmName("convertListPExpression")
fun LinkedList<PExpression>?.convert(): List<Expression> {
    return this?.mapNotNull { it.convert() } ?: emptyList()
}

/**
 * Converts a list of BParser nodes to a list of intermediate code representations.
 *
 * @return A list of intermediate code representations
 */
@JvmName("convertListPPredicate")
fun LinkedList<PPredicate>?.convert(): List<Predicate> {
    return this?.mapNotNull { it.convert() } ?: emptyList()
}

/**
 * Converts a list of BParser nodes to a list of intermediate code representations.
 *
 * @return A list of intermediate code representations
 */
@JvmName("convertListPOperation")
fun LinkedList<POperation>?.convert(): List<Operation> {
    return this?.mapNotNull { it.convert() } ?: emptyList()
}

/**
 * Converts a list of BParser nodes to a list of intermediate code representations.
 *
 * @return A list of intermediate code representations
 */
@JvmName("convertListPSubstitution")
fun LinkedList<PSubstitution>?.convert(): List<Substitution> {
    return this?.mapNotNull { it.convert() } ?: emptyList()
}

/**
 * Converts a list of BParser nodes to a list of intermediate code representations.
 *
 * @return A list of intermediate code representations
 */
@JvmName("convertListPSet")
fun LinkedList<PSet>?.convert(): List<CollectionNode> {
    return this?.mapNotNull { it.convert() } ?: emptyList()
}

/**
 * Converts a BParser PExpression node to an intermediate code representation.
 *
 * @return The intermediate code representation
 */
fun PExpression?.convert(): Expression? {
    if (this == null) return null

    val visitor = ExpressionVisitor()
    this.apply(visitor)
    return visitor.result
}

/**
 * Converts a BParser PPredicate node to an intermediate code representation.
 *
 * @return The intermediate code representation
 */
fun PPredicate?.convert(): Predicate? {
    if (this == null) return null

    val visitor = PredicateVisitor()
    this.apply(visitor)
    return visitor.result
}

/**
 * Converts a BParser POperation node to an intermediate code representation.
 *
 * @return The intermediate code representation
 */
fun POperation?.convert(): Operation? {
    if (this == null) return null

    val visitor = OperationVisitor()
    this.apply(visitor)
    return visitor.result
}

/**
 * Converts a BParser PSubstitution node to an intermediate code representation.
 *
 * @return The intermediate code representation
 */
fun PSubstitution?.convert(): Substitution? {
    if (this == null) return null

    val visitor = SubstitutionVisitor()
    this.apply(visitor)
    return visitor.result
}

/**
 * Converts a BParser PExpression node to an intermediate code representation.
 * Returns null, if the node is null.
 *
 * @return The intermediate code representation or null
 */
fun PSubstitution?.convertOrNull(): Substitution? {
    if (this == null) return null
    return this.convert()
}

/**
 * Converts a BParser PSet node to an intermediate code representation.
 *
 * @return The intermediate code representation
 */
fun PSet?.convert(): CollectionNode? {
    if (this == null) return null

    val visitor = CollectionVisitor()
    this.apply(visitor)
    return visitor.result
}

