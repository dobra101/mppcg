package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.AOperation
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeVoid
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.IdentifierExpression

class OperationVisitor : AbstractVisitor() {

    override var result: Operation? = null

    companion object {
        var returnValues: List<Expression> = emptyList()
        var operationType: Type = TypeVoid()
        var declaredOrKnown: Set<IdentifierExpression> = emptySet()
    }

    override fun caseAOperation(node: AOperation) {
        returnValues = node.returnValues.convert()
        operationType = TypeVoid()
        declaredOrKnown = knownIdentifier()

        // TODO: when more than one name?
        result = Operation(
            name = node.opName[0].text,
            parameters = node.parameters.convert(),
            returnValues = returnValues,
            body = node.operationBody.convert(),
            type = operationType
        )
    }

    private fun knownIdentifier(): Set<IdentifierExpression> {
        val known: MutableList<Expression> = mutableListOf()
        known += machineVisitor.variables
        known += machineVisitor.constants
        known += machineVisitor.concreteConstants
        known += machineVisitor.concreteVariables
        return known.filterIsInstance<IdentifierExpression>().toSet()
    }
}