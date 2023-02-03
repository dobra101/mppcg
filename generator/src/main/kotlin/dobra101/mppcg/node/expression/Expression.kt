package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type

abstract class Expression(open var type: Type? = null, override val templateName: String) : MPPCGNode