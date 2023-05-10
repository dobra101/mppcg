package dobra101.mppcg.node.expression

import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.Type

// TODO: type not nullable but lateinit? -> or by default TypeVariable()?
abstract class Expression(override val templateName: String, open var type: Type? = null) : MPPCGNode {
    val node: Expression
        get() = this
}