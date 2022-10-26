package dobra101.mppcg.node.b

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate

data class Machine(
    val name: String,
    val parameters: List<Expression> = emptyList(),
    val constraints: Predicate? = null,
    val sets: List<CollectionNode> = emptyList(),
    val constants: List<Expression> = emptyList(),
    val concreteConstants: List<Expression> = emptyList(),
    val properties: Predicate? = null,
    val definitions: Predicate? = null,
    val variables: List<Expression> = emptyList(),
    val concreteVariables: List<Expression> = emptyList(),
    val initialization: Initialization? = null,
    val invariant: Predicate? = null,
    val assertions: List<Predicate> = emptyList(),
    val operations: List<Operation> = emptyList()
) : MPPCGNode {

    // TODO: only needed when model checking ?
    val transitions = operations.mapNotNull {
        val pre = when (it.body) {
            is Precondition -> it.body.predicate
            is Select -> it.body.condition
            else -> null
        }
        if (pre != null) Transition(it.name, pre)
        else null
    }
}