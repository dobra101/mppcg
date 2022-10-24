package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type

data class IdentifierExpression(val name: String, val type: Type) : MPPCGNode
