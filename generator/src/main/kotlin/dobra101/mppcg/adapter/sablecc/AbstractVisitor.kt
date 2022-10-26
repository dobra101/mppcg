package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.analysis.DepthFirstAdapter
import de.be4.classicalb.core.parser.node.Node
import dobra101.mppcg.node.MPPCGNode

/**
 * Does basically the same as the DepthFirstAdapter, but carries a resulting node that can be retrieved
 * when the visitor finished.
 */
abstract class AbstractVisitor: DepthFirstAdapter() {
    abstract val result: MPPCGNode?

    override fun defaultCase(node: Node) {
        throw VisitorException("Not implemented ${node::class}")
    }
}

class VisitorException(msg: String): Exception(msg)