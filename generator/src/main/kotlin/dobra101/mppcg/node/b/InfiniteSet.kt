package dobra101.mppcg.node.b

import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.Type

data class InfiniteSet(
    var setType: Type // TODO: needed?
) : Expression("infiniteSetExpression")