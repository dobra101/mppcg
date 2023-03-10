package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.b.BinaryFunctionExpression
import dobra101.mppcg.node.b.CallFunctionExpression
import dobra101.mppcg.node.b.Couple
import dobra101.mppcg.node.collection.AnonymousSetCollectionNode
import dobra101.mppcg.node.collection.EnumEntry
import dobra101.mppcg.node.expression.BinaryExpression
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.expression.ValueExpression

data class ParallelSubstitution(
    val substitutions: List<Substitution>
) : Substitution("parallelSubstitution") {
    val needTempVar: Set<IdentifierExpression>

    init {
        val assignments = substitutions.filterIsInstance<AssignSubstitution>()

        val identifierOnRhs = assignments.withIndex().associate { (idx, assign) -> idx to assign.right.getIdentifiers() }

        val tempVars = mutableSetOf<IdentifierExpression>()
        // store identifiers which occur on both sides and are not only self-assigning
        // only those are relevant, which are assigned first and later used on a right side // TODO: java?
        for ((idx, assignment) in assignments.withIndex()) {
            if (assignment.left !is IdentifierExpression) continue
            if (tempVars.contains(assignment.left)) continue

            // get indices of assignments containing identifier on lhs
            val indices = identifierOnRhs.getIndicesOfIdentifier(assignment.left).toMutableList()
            indices.removeAll { it <= idx }
            if (indices.isNotEmpty()) {
                tempVars.add(assignment.left)
            }
        }
        needTempVar = tempVars
    }

    private fun Map<Int, List<IdentifierExpression>>.getIndicesOfIdentifier(identifier: IdentifierExpression): List<Int> {
        return filter { it.value.contains(identifier) }.map { it.key }
    }

    // TODO: duplicate
    private fun Expression.getIdentifiers(): List<IdentifierExpression> {
        return when (this) {
            is IdentifierExpression -> listOf(this)
            is ValueExpression -> listOf()
            is EnumEntry -> listOf()
            is Couple -> from.getIdentifiers() + to.getIdentifiers()
            is AnonymousSetCollectionNode -> elements.flatMap { it.getIdentifiers() }
            is BinaryExpression -> left.getIdentifiers() + right.getIdentifiers()
            is BinaryFunctionExpression -> left.getIdentifiers() + right.getIdentifiers()
            is CallFunctionExpression -> expression.getIdentifiers() + parameters.flatMap { it.getIdentifiers() }
            else -> TODO("Implement getIdentifier from Expression ${this::class.simpleName}")
        }
    }
}
