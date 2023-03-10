package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.BooleanValue
import dobra101.mppcg.node.TypeBoolean
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
        val predicate = node.implication.convert()!!
        val identifier = node.identifiers.convert().map { it as IdentifierExpression }
        result = if (predicate is BinaryLogicPredicate && predicate.operator == LogicPredicateOperator.IMPLIES) {
            QuantifierPredicate(
                identifier = identifier,
                predicate = predicate.left,
                quantification = predicate.right,
                type = QuantifierType.FORALL
            )
        } else {
            QuantifierPredicate(identifier = identifier, predicate = predicate, type = QuantifierType.FORALL)
        }
    }

    override fun caseAExistsPredicate(node: AExistsPredicate) {
        val predicate = node.predicate.convert()!!
        val identifier = node.identifiers.convert().map { it as IdentifierExpression }
        result = if (predicate is BinaryLogicPredicate && predicate.operator == LogicPredicateOperator.AND) {
            QuantifierPredicate(
                identifier = identifier,
                predicate = predicate.left,
                quantification = predicate.right,
                type = QuantifierType.EXISTS
            )
        } else {
            QuantifierPredicate(identifier = identifier, predicate = predicate, type = QuantifierType.EXISTS)
        }
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
        val pred = BinaryPredicate(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryPredicateOperator.SUBSET
        )
        result = UnaryLogicPredicate(pred, LogicPredicateOperator.NOT)
    }

    override fun caseANotSubsetStrictPredicate(node: ANotSubsetStrictPredicate) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATruthPredicate(node: ATruthPredicate) {
        result = ValuePredicate("", type = TypeBoolean(BooleanValue.TRUE))
    }

    override fun caseAFalsityPredicate(node: AFalsityPredicate) {
        result = ValuePredicate("", type = TypeBoolean(BooleanValue.FALSE))
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