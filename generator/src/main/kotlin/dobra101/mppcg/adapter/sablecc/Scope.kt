package dobra101.mppcg.adapter.sablecc

import dobra101.mppcg.node.expression.Expression

data class Scope(val parent: Scope? = null, val inner: MutableList<Scope> = mutableListOf()) {
    val known: MutableList<Expression> = mutableListOf()

    init {
        parent?.inner?.add(this)
        parent?.known?.let { known.addAll(it) }
    }
}