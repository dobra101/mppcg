package dobra101.mppcg.node

import dobra101.mppcg.node.expression.Expression

data class ClassVariables(
    val variables: List<Expression> = emptyList(),
    override val templateName: String = "classVariables"
): MPPCGNode