package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type

abstract class Expression(override val templateName: String, open var type: Type? = null) : MPPCGNode {
    val node: Expression
        get() = this
}