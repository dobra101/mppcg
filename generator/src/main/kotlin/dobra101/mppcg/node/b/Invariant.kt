package dobra101.mppcg.node.b

import dobra101.mppcg.node.predicate.BinaryLogicPredicate
import dobra101.mppcg.node.predicate.LogicPredicateOperator
import dobra101.mppcg.node.predicate.Predicate

data class Invariant(
    val predicates: List<Predicate> = emptyList()
) : Predicate("invariant") {
    companion object {
        fun of(predicate: Predicate?): Invariant {
            if (predicate == null) return Invariant()

            if (predicate !is BinaryLogicPredicate) {
                return Invariant(listOf(predicate))
            }

            if (predicate.operator != LogicPredicateOperator.AND) {
                return Invariant(listOf(predicate))
            }

            val list: MutableList<Predicate> = mutableListOf()
            list.addAll(predicate.left.asList())
            list.addAll(predicate.right.asList())

            return Invariant(list)
        }
    }
}