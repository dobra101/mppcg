package dobra101.mppcg.environment

import dobra101.mppcg.node.MPPCGNode

/**
 * Enables optimization strategies for rendering Prolog.
 * These optimizations lead to a reduced amount of lines of generated code.
 */
class EvaluatedExpressions : HashMap<MPPCGNode, String>() {

    /**
     * Adds a node to the map.
     *
     * @param node The node to add
     * @param evaluated The string to render when this node is visited again
     */
    fun add(node: MPPCGNode, evaluated: String) {
        this[node] = evaluated
    }

    /**
     * Executes a block of code and (optionally) resets the map afterward.
     * This is used to avoid storing specific nodes in the map.
     *
     * @param resetCondition The condition to determine if the map is being reset to the previous state after execution
     * @param blockToReset A block of code to execute, whose nodes are not supposed to be stored inside the map.
     */
    fun withReset(resetCondition: Boolean = true, blockToReset: () -> Unit) {
        val before = HashMap(this)

        blockToReset()

        if (resetCondition) {
            clear()
            putAll(before)
        }
    }

    /**
     * Executes a block of code and returns its value.
     * Optionally, the block of code is being executed with a fresh map.
     * This is to avoid optimized rendering of nodes at positions, where the non-optimized version is required.
     *
     * After the execution, the map is reset to the state prior the execution.
     *
     * @param clearBefore True, if the blockToReset should be evaluated with a fresh map
     * @param blockToReset A block of code to execute
     *
     * @return A generic return value which is returned by the executed block of code
     */
    fun <T> returnWithReset(clearBefore: Boolean = false, blockToReset: () -> T): T {
        val before = HashMap(this)

        if (clearBefore) clear()

        val result = blockToReset()

        clear()
        putAll(before)

        return result
    }

    /**
     * Conditioned version of [returnWithReset]. See also [withReset].
     *
     * @param resetCondition The condition to determine if the map is being reset to the previous state after execution
     * @param blockToReset A block of code to execute, whose nodes are not supposed to be stored inside the map
     * @return A generic return value which is returned by the executed block of code
     */
    fun <T> returnWithConditionedReset(resetCondition: Boolean = true, blockToReset: () -> T): T {
        val before = HashMap(this)

        val result = blockToReset()

        if (resetCondition) {
            clear()
            putAll(before)
        }

        return result
    }
}