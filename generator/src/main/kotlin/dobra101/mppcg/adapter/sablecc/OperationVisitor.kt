package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.AOperation
import dobra101.mppcg.node.b.Operation

class OperationVisitor : AbstractVisitor() {

    override var result: Operation? = null

    override fun caseAOperation(node: AOperation) {
        // TODO: when more than one name?
        result = Operation(
            name = node.opName[0].text,
            parameters = node.parameters.convert(),
            returnValues = node.returnValues.convert(),
            body = node.operationBody.convert()
        )
    }
}