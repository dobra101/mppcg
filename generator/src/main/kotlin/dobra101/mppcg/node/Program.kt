package dobra101.mppcg.node

/**
 * The base node for every root node of a program.
 */
interface Program: MPPCGNode {
    val name: String

    /**
     * Implemented by each subtype to execute for example keyword handling.
     */
    fun getAllNodesWithIdentifiers(): List<MPPCGNode>
}