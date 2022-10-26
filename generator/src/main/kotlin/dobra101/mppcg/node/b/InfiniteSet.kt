package dobra101.mppcg.node.b

import dobra101.mppcg.node.TypeSet
import dobra101.mppcg.node.expression.Expression

data class InfiniteSet(
    override var type: TypeSet
) : Expression(type)