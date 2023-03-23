package dobra101.mppcg.adapter.sablecc

import dobra101.mppcg.node.expression.Expression

data class Scope(val parent: Scope? = null, val inner: MutableList<Scope> = mutableListOf()) {
    val known: MutableList<Expression> = mutableListOf()
        get() {
            if (parent == null) return field
            return (parent.known + field).toMutableList()
        }

    init {
        parent?.inner?.add(this)
    }
}