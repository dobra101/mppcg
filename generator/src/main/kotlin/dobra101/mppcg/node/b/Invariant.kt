package dobra101.mppcg.node.b

import dobra101.mppcg.node.predicate.LogicPredicate
import dobra101.mppcg.node.predicate.LogicPredicateOperator
import dobra101.mppcg.node.predicate.Predicate

data class Invariant(
    val predicates: List<Predicate> = emptyList()
) : Predicate("invariant") {
    companion object {
        fun of(predicate: Predicate?): Invariant {
            if (predicate == null) return Invariant()

            if (predicate !is LogicPredicate) {
                throw InvariantException("Predicate is not a LogicPredicate. (${predicate::class.simpleName})")
            }

            if (predicate.operator != LogicPredicateOperator.AND) {
                throw InvariantException("Can not split invariant at operator ${predicate.operator}")
            }

            val list: MutableList<Predicate> = mutableListOf()
            list.addAll(predicate.left.asList())
            list.addAll(predicate.right.asList())

            return Invariant(list)
        }

        private fun Predicate.asList(): List<Predicate> {
            if (this !is LogicPredicate) return listOf(this)

            if (operator != LogicPredicateOperator.AND) return listOf(this)

            val list: MutableList<Predicate> = mutableListOf()
            list.addAll(left.asList())
            list.addAll(right.asList())

            return list
        }
    }
}

class InvariantException(msg: String) : Exception(msg)