package dobra101.mppcg.adapter.sablecc

import dobra101.mppcg.node.expression.Expression

class ExpressionVisitor : AbstractVisitor() {
    override var result: Expression? = null
}