package dobra101.mppcg.node.b

import dobra101.mppcg.node.TypeCollection
import dobra101.mppcg.node.collection.CollectionType
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate

data class ComprehensionSet(
    val identifiers: List<Expression>,
    val predicates: Predicate
) : Expression(TypeCollection(type = CollectionType.Set), "comprehensionSetExpression")