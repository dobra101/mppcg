package dobra101.mppcg.node

import dobra101.mppcg.Generator.Companion.environment
import dobra101.mppcg.RenderResult

interface MPPCGNode {
    /**
     * Renders the node to a source code string.
     *
     * @return A RenderResult containing the resulting string and individual information
     */
    fun render(): RenderResult {
        return environment.delegateCall(this)
    }

    val templateName: String

    fun findAllIdentifiers(): List<MPPCGNode> {
        val node = MPPCGNode::class.java
        val collection = Collection::class.java
        val string = String::class.java
        // TODO: not working because of private final, see type inference
        javaClass.declaredFields.forEach {
            // TODO: excludes inherited fields, use javaClass.fields
            if (node.isAssignableFrom(it.type)) {
                println("IS NODE: $it")
            } else if (collection.isAssignableFrom(it.type)) {
                println("IS LIST: $it")
            } else if (string.isAssignableFrom(it.type)){
                println("IS STRING: $it")
            } else {
                println("CANT GET IDENTIFIERS OF TYPE: ${it.type}")
            }
        }
        return listOf()
    }
}