package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.b.Initialization
import dobra101.mppcg.node.b.Precondition
import dobra101.mppcg.node.b.Select
import dobra101.mppcg.node.substitution.AssignSubstitution
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
}