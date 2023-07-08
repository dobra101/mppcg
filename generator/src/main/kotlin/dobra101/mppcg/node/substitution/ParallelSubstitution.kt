package dobra101.mppcg.node.substitution

import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.collection.AnonymousCollectionNode
import dobra101.mppcg.node.collection.BinaryCollectionExpression
import dobra101.mppcg.node.collection.EnumCollectionNode
import dobra101.mppcg.node.collection.EnumEntry
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.BinaryLogicPredicate
import dobra101.mppcg.node.predicate.BinaryPredicate
import dobra101.mppcg.node.predicate.Predicate

data class ParallelSubstitution(
    val substitutions: List<Substitution>
) : Substitution("parallelSubstitution") {
    val needTempVar: Set<IdentifierExpression>

    // Calculate which temporary variables are required
    init {
        val assignments = substitutions.filterIsInstance<AssignSubstitution>()

        val identifierOnRhs = substitutions.withIndex()
            .filter { (_, subs) -> subs is AssignSubstitution }
            .associate { (idx, assign) -> idx to (assign as AssignSubstitution).right.getIdentifiers() }

        val identifierInIfs = substitutions.withIndex()
            .filter { (_, subs) -> subs is IfSubstitution }
            .associate { (idx, ifSubs) -> idx to (ifSubs as IfSubstitution).getIdentifiers() }

        val allIdentifiers = identifierOnRhs + identifierInIfs

        val tempVars = mutableSetOf<IdentifierExpression>()
        // store identifiers which occur on both sides and are not only self-assigning
        // only those are relevant, which are assigned first and later used on a right side
        for ((idx, assignment) in assignments.withIndex()) {
            if (assignment.left !is IdentifierExpression) continue
            if (tempVars.contains(assignment.left)) continue

            // get indices of assignments containing identifier on lhs
            val indices = allIdentifiers.getIndicesOfIdentifier(assignment.left).toMutableList()
            indices.removeAll { it <= idx }
            if (indices.isNotEmpty()) {
                tempVars.add(assignment.left)
            }
        }
        needTempVar = tempVars
    }

    // TODO: make generic
    private fun Map<Int, List<IdentifierExpression>>.getIndicesOfIdentifier(identifier: IdentifierExpression): List<Int> {
        return filter { it.value.contains(identifier) }.map { it.key }
    }

    private fun Substitution.getIdentifiers(): List<IdentifierExpression> {
        return when (this) {
            is IfSubstitution ->
                condition.getIdentifiers() +
                        then.getIdentifiers() +
                        elseIf.flatMap { it.getIdentifiers() } +
                        elseSubstitution?.getIdentifiers().let { it ?: listOf() }

            is ElseIfSubstitution -> condition.getIdentifiers() + then.getIdentifiers()
            is ParallelSubstitution -> substitutions.flatMap { it.getIdentifiers() }
            is AssignSubstitution -> left.getIdentifiers() + right.getIdentifiers()
            else -> TODO("Implement getIdentifier from Substitution ${this::class.simpleName}")
        }
    }

    private fun Predicate.getIdentifiers(): List<IdentifierExpression> {
        return when (this) {
            is BinaryPredicate -> left.getIdentifiers() + right.getIdentifiers()
            is BinaryLogicPredicate -> left.getIdentifiers() + right.getIdentifiers()
            else -> TODO("Implement getIdentifier from Predicate ${this::class.simpleName}")
        }
    }

    // TODO: duplicate
    private fun Expression.getIdentifiers(): List<IdentifierExpression> {
        return when (this) {
            is IdentifierExpression -> listOf(this)
            is ValueExpression -> listOf()
            is EnumEntry -> listOf()
            is Couple -> from.getIdentifiers() + to.getIdentifiers()
            is AnonymousCollectionNode -> elements.flatMap { it.getIdentifiers() }
            is BinaryExpression -> left.getIdentifiers() + right.getIdentifiers()
            is BinaryFunctionExpression -> left.getIdentifiers() + right.getIdentifiers()
            is CallFunctionExpression -> expression.getIdentifiers() + parameters.flatMap { it.getIdentifiers() }
            is ConcreteIdentifierExpression -> listOf(IdentifierExpression(name))
            is BinaryCollectionExpression -> left.getIdentifiers() + right.getIdentifiers()
            is UnaryFunctionExpression -> expression.getIdentifiers()
            is EnumCollectionNode -> listOf(IdentifierExpression(name)) + elements.flatMap { it.getIdentifiers() }
            is UnaryExpression -> (value as? Expression)?.getIdentifiers() ?: listOf()
            else -> TODO("Implement getIdentifier from Expression ${this::class.simpleName}")
        }
    }
}
