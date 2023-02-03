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
        return environment.call(this)
    }

    val templateName: String
}