package dobra101.mppcg.node.b

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeVoid
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.substitution.Substitution

data class Operation(
    override var name: String,
    val parameters: List<Expression> = emptyList(),
    val returnValues: List<Expression> = emptyList(),
    val body: Substitution? = null,
    val type: Type = TypeVoid(),
    override val templateName: String = "operation"
) : MPPCGNode, NamedNode