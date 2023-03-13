package dobra101.mppcg.node.b

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate

data class Transition(
    override var name: String,
    val parameters: List<Expression>,
    val body: Predicate,
    override val templateName: String = "transition"
) : MPPCGNode, NamedNode