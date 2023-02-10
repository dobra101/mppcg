package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.analysis.DepthFirstAdapter
import de.be4.classicalb.core.parser.node.Node
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.expression.Expression
import java.io.FileInputStream
import java.io.IOException
import java.util.logging.LogManager
import java.util.logging.Logger

/**
 * Does basically the same as the DepthFirstAdapter, but carries a resulting node that can be retrieved
 * when the visitor finished.
 */
abstract class AbstractVisitor: DepthFirstAdapter() {
    companion object {
        var result: MPPCGNode? = null
    }
    abstract val result: MPPCGNode?
    protected val logger: Logger = Logger.getLogger(this::class.simpleName)

    init {
        val manager = LogManager.getLogManager()
        try {
            manager.readConfiguration(FileInputStream("logging.properties"))
        } catch (e: IOException) {
            logger.warning(e.message)
        }
    }

    override fun defaultCase(node: Node) {
        throw VisitorException("Not implemented ${node::class}")
    }

    protected fun Expression.setParameterIfCollection(): Expression {
        if (this is CollectionNode) {
            this.isParameter = true
        }
        return this
    }
}

class VisitorException(msg: String): Exception(msg)