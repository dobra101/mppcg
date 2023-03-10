package dobra101.mppcg.node

interface Program: MPPCGNode {
    val name: String

    // TODO: doc
    // needed as starting point
    fun getAllIdentifiers(): List<MPPCGNode>
}