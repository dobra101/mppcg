package dobra101.mppcg.node.b

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.expression.Expression

data class InfiniteSet(
    override var type: Type?
) : Expression(type)