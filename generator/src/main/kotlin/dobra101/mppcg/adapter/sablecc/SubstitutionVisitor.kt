package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.b.Initialization
import dobra101.mppcg.node.b.Precondition
import dobra101.mppcg.node.b.Select
import dobra101.mppcg.node.substitution.AssignSubstitution
import dobra101.mppcg.node.substitution.ParallelSubstitution
import dobra101.mppcg.node.substitution.Substitution

class SubstitutionVisitor : AbstractVisitor() {

    override var result: Substitution? = null

    override fun caseAAssignSubstitution(node: AAssignSubstitution) {
        result = AssignSubstitution(node.lhsExpression.convert(), node.rhsExpressions.convert())
    }

    override fun caseAPreconditionSubstitution(node: APreconditionSubstitution) {
        result = Precondition(node.substitution.convert()!!, node.predicate.convert()!!)
    }

    override fun caseASelectSubstitution(node: ASelectSubstitution) {
        result = Select(
            condition = node.condition.convert()!!,
            then = node.then.convert()!!,
            whenSubstitution = node.whenSubstitutions.convert(),
            elseSubstitution = node.`else`.convertOrNull()
        )
    }

    override fun caseASelectWhenSubstitution(node: ASelectWhenSubstitution) {
        TODO("Not implemented caseASelectWhenSubstitution")
    }

    override fun caseASequenceSubstitution(node: ASequenceSubstitution) {
        result = Initialization(node.substitutions.mapNotNull { it.convert() })
    }

    override fun caseABlockSubstitution(node: ABlockSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASkipSubstitution(node: ASkipSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAAssertionSubstitution(node: AAssertionSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAChoiceSubstitution(node: AChoiceSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAChoiceOrSubstitution(node: AChoiceOrSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIfSubstitution(node: AIfSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIfElsifSubstitution(node: AIfElsifSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACaseSubstitution(node: ACaseSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACaseOrSubstitution(node: ACaseOrSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAAnySubstitution(node: AAnySubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALetSubstitution(node: ALetSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABecomesElementOfSubstitution(node: ABecomesElementOfSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABecomesSuchSubstitution(node: ABecomesSuchSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAVarSubstitution(node: AVarSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFuncOpSubstitution(node: AFuncOpSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOpSubstitution(node: AOpSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperationCallSubstitution(node: AOperationCallSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAWhileSubstitution(node: AWhileSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAParallelSubstitution(node: AParallelSubstitution) {
        result = ParallelSubstitution(node.substitutions.convert())
    }

    override fun caseADefinitionSubstitution(node: ADefinitionSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInvalidSubstitution(node: AInvalidSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAForallSubMessageSubstitution(node: AForallSubMessageSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARuleFailSubSubstitution(node: ARuleFailSubSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAForLoopSubstitution(node: AForLoopSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperatorSubstitution(node: AOperatorSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADefineSubstitution(node: ADefineSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }
}