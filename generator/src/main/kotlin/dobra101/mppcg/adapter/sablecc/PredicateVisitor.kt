package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.predicate.*

class PredicateVisitor : AbstractVisitor() {

    override var result: Predicate? = null

    override fun caseALessPredicate(node: ALessPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.LESS)
    }

    override fun caseALessEqualPredicate(node: ALessEqualPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.LESS_EQUAL)
    }

    override fun caseAGreaterPredicate(node: AGreaterPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.GREATER)
    }

    override fun caseAGreaterEqualPredicate(node: AGreaterEqualPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.GREATER_EQUAL)
    }

    override fun caseAEqualPredicate(node: AEqualPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.EQUAL)
    }

    override fun caseANotEqualPredicate(node: ANotEqualPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.NOT_EQUAL)
    }

    override fun caseAMemberPredicate(node: AMemberPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.MEMBER)
    }

    override fun caseANotMemberPredicate(node: ANotMemberPredicate) {
        result = BinaryPredicate(node.left.convert()!!, node.right.convert()!!, BinaryPredicateOperator.NOT_MEMBER)
    }

    override fun caseAConjunctPredicate(node: AConjunctPredicate) {
        result = LogicPredicate(node.left.convert()!!, node.right.convert()!!, LogicPredicateOperator.AND)
    }

    override fun caseADisjunctPredicate(node: ADisjunctPredicate) {
        result = LogicPredicate(node.left.convert()!!, node.right.convert()!!, LogicPredicateOperator.OR)
    }
}