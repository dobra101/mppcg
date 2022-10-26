package dobra101.mppcg.adapter.sablecc

import dobra101.mppcg.node.b.Operation

class OperationVisitor : AbstractVisitor() {
    override var result: Operation? = null
}