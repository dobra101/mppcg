package dobra101.mppcg.node.b

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeSet
import dobra101.mppcg.node.expression.Expression

data class InfiniteSet(
    var setType: Type
) : Expression(TypeSet(setType), "infiniteSetExpression")