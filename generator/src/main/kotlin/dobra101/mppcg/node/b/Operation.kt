package dobra101.mppcg.node.b

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.substitution.Substitution

data class Operation(
    val name: String,
    val parameters: List<Expression> = emptyList(),
    val returnValues: List<Expression> = emptyList(),
    val body: Substitution? = null,
    override val templateName: String = "operation"
) : MPPCGNode