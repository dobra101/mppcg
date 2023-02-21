package dobra101.mppcg.node.predicate

import dobra101.mppcg.node.MPPCGNode

abstract class Predicate(override val templateName: String) : MPPCGNode {
    fun asList(): List<Predicate> {
        if (this !is BinaryLogicPredicate) return listOf(this)

        if (operator != LogicPredicateOperator.AND) return listOf(this)

        val list: MutableList<Predicate> = mutableListOf()
        list.addAll(left.asList())
        list.addAll(right.asList())

        return list
    }
}