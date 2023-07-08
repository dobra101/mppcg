package dobra101.mppcg.node

/**
 * The base node for every root node of a program.
 */
interface Program: MPPCGNode, NamedNode {
    override var name: String
}