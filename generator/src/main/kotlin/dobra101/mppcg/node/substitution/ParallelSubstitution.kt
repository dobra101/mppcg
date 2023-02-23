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

        val identifierOnLhs = assignments.mapIndexed { idx, assignment ->
            idx to assignment.lhs.filterIsInstance<IdentifierExpression>()
        }.toMap()

        val identifierOnRhs = assignments.mapIndexed { idx, assignment ->
            val rawIdentifierOnRhs = assignment.rhs.filterIsInstance<IdentifierExpression>()
            val innerIdentifierOnRhs = assignment.rhs.flatMap { it.getIdentifier() }
            idx to rawIdentifierOnRhs + innerIdentifierOnRhs
        }.toMap()

        val tempVars = mutableSetOf<IdentifierExpression>()
        // store identifiers which occur on both sides and are not only self-assigning
        // only those are relevant, which are assigned first and later used on a right side
        for ((idx, identifierList) in identifierOnLhs) {
            for (identifier: IdentifierExpression in identifierList) {
                if (tempVars.contains(identifier)) continue

                val indices = identifierOnRhs.getIndicesOfIdentifier(identifier).toMutableList()
                indices.removeAll { it <= idx }
                if (indices.isNotEmpty()) {
                    tempVars.add(identifier)
                }
            }
        }
        needTempVar = tempVars
    }

    private fun Map<Int, List<IdentifierExpression>>.getIndicesOfIdentifier(identifier: IdentifierExpression): List<Int> {
        return filter { it.value.contains(identifier) }.map { it.key }
    }

    private fun Expression.getIdentifier(): List<IdentifierExpression> {
        return when (this) {
            is IdentifierExpression -> listOf(this)
            is ValueExpression -> listOf()
            is EnumEntry -> listOf()
            is Couple -> from.getIdentifier() + to.getIdentifier()
            is AnonymousSetCollectionNode -> elements.flatMap { it.getIdentifier() }
            is BinaryExpression -> left.getIdentifier() + right.getIdentifier()
            is BinaryFunctionExpression -> left.getIdentifier() + right.getIdentifier()
            is CallFunctionExpression -> expression.getIdentifier() + parameters.flatMap { it.getIdentifier() }
            else -> TODO("Implement getIdentifier from Expression ${this::class.simpleName}")
        }
    }
}
