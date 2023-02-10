package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.expression.IdentifierExpression
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
        result = BinaryPredicate(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryPredicateOperator.MEMBER
        )
    }

    override fun caseANotMemberPredicate(node: ANotMemberPredicate) {
        result = BinaryPredicate(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryPredicateOperator.NOT_MEMBER
        )
    }

    override fun caseAConjunctPredicate(node: AConjunctPredicate) {
        result = BinaryLogicPredicate(node.left.convert()!!, node.right.convert()!!, LogicPredicateOperator.AND)
    }

    override fun caseADisjunctPredicate(node: ADisjunctPredicate) {
        result = BinaryLogicPredicate(node.left.convert()!!, node.right.convert()!!, LogicPredicateOperator.OR)
    }

    override fun caseASubsetPredicate(node: ASubsetPredicate) {
        result = BinaryPredicate(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryPredicateOperator.SUBSET
        )
    }

    override fun caseAForallPredicate(node: AForallPredicate) {
        // TODO: when more than one identifier?
        result = QuantifierPredicate(
            identifier = node.identifiers.convert()[0] as IdentifierExpression,
            predicate = node.implication.convert()!!,
            type = QuantifierType.FORALL
        )
    }

    override fun caseAExistsPredicate(node: AExistsPredicate) {
        // TODO: when more than one identifier?
        result = QuantifierPredicate(
            identifier = node.identifiers.convert()[0] as IdentifierExpression,
            predicate = node.predicate.convert()!!,
            type = QuantifierType.EXISTS
        )
    }

    override fun caseADescriptionPredicate(node: ADescriptionPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALabelPredicate(node: ALabelPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASubstitutionPredicate(node: ASubstitutionPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseANegationPredicate(node: ANegationPredicate) {
        result = UnaryLogicPredicate(node.predicate.convert()!!, LogicPredicateOperator.NOT)
    }

    override fun caseAImplicationPredicate(node: AImplicationPredicate) {
        result = BinaryLogicPredicate(node.left.convert()!!, node.right.convert()!!, LogicPredicateOperator.IMPLIES)
    }

    override fun caseAEquivalencePredicate(node: AEquivalencePredicate) {
        result = BinaryLogicPredicate(node.left.convert()!!, node.right.convert()!!, LogicPredicateOperator.EQUIVALENCE)
    }

    override fun caseASubsetStrictPredicate(node: ASubsetStrictPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseANotSubsetPredicate(node: ANotSubsetPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseANotSubsetStrictPredicate(node: ANotSubsetStrictPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATruthPredicate(node: ATruthPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFalsityPredicate(node: AFalsityPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFinitePredicate(node: AFinitePredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPartitionPredicate(node: APartitionPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADefinitionPredicate(node: ADefinitionPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPredicateIdentifierPredicate(node: APredicateIdentifierPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPredicateFunctionPredicate(node: APredicateFunctionPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALetPredicatePredicate(node: ALetPredicatePredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIfPredicatePredicate(node: AIfPredicatePredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAExtendedPredPredicate(node: AExtendedPredPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperatorPredicate(node: AOperatorPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }
}