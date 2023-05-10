package dobra101.mppcg.node.b

import dobra101.mppcg.node.*
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.substitution.Substitution

data class Operation(
    override var name: String,
    val parameters: List<Expression> = emptyList(),
    val returnValues: List<Expression> = emptyList(),
    val body: Substitution? = null,
    var type: Type = MPPCG_Void,
    override val templateName: String = "operation"
) : MPPCGNode, NamedNode