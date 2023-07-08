package dobra101.mppcg.node.b

import dobra101.mppcg.node.ClassVariables
import dobra101.mppcg.node.Program
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate

data class Machine(
    override var name: String,
    val parameters: List<Expression> = emptyList(),
    val constraints: Predicate? = null,
    val sets: List<CollectionNode> = emptyList(),
    val constants: List<Expression> = emptyList(),
    val concreteConstants: List<Expression> = emptyList(),
    val properties: List<Predicate> = emptyList(),
    val definitions: Predicate? = null,
    val variables: ClassVariables = ClassVariables(),
    val concreteVariables: List<Expression> = emptyList(),
    val initialization: Initialization = Initialization(),
    val invariant: Invariant = Invariant(),
    val assertions: List<Predicate> = emptyList(),
    val operations: List<Operation> = emptyList(),
    override val templateName: String = "machine"
) : Program {

    val transitions = operations.mapNotNull {
        val pre = when (it.body) {
            is Precondition -> it.body.predicate
            is Select -> it.body.condition
            else -> null
        }
        if (pre != null) Transition(it.name, it.parameters, pre)
        else null
    }
}