package dobra101.mppcg.environment

import dobra101.mppcg.node.MPPCGNode

class EvaluatedExpressions : HashMap<MPPCGNode, String>() {

    fun add(node: MPPCGNode, evaluated: String) {
        this[node] = evaluated
    }

    fun withReset(resetCondition: Boolean = true, blockToReset: () -> Unit) {
        val before = HashMap(this)

        blockToReset()

        if (resetCondition) {
            clear()
            putAll(before)
        }
    }

    fun <T> returnWithReset(clearBefore: Boolean = false, blockToReset: () -> T): T {
        val before = HashMap(this)

        if (clearBefore) clear()

        val result = blockToReset()

        clear()
        putAll(before)

        return result
    }

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