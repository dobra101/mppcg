package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.AOperation
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.TypeVoid
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.expression.Expression

class OperationVisitor : AbstractVisitor() {

    override var result: Operation? = null

    companion object {
        var returnValues: List<Expression> = emptyList()
        var parameters: List<Expression> = emptyList()
        var operationType: Type = TypeVoid()
    }

    override fun caseAOperation(node: AOperation) {
        machineVisitor.currentScope = Scope(machineVisitor.scope)

        returnValues = node.returnValues.convert()
        parameters = node.parameters.convert()
        machineVisitor.recognize(parameters)
        operationType = TypeVoid()

        // TODO: when more than one name?
        result = Operation(
            name = node.opName[0].text,
            parameters = parameters,
            returnValues = returnValues,
            body = node.operationBody.convert(),
            type = operationType
        )
        machineVisitor.currentScope = machineVisitor.currentScope.parent!!
    }
}