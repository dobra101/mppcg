package dobra101.mppcg.node.b

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeSequence
import dobra101.mppcg.node.expression.Expression

data class Sequence(
    val elements: List<Expression> = emptyList(),
    private var sequenceType: Type? = null
) : Expression(type = TypeSequence(sequenceType), "sequence")