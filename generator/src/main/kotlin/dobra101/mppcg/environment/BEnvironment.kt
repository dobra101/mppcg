package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.IntersectionCollectionExpression
import dobra101.mppcg.node.collection.SubtractionCollectionExpression
import dobra101.mppcg.node.collection.UnionCollectionExpression
import dobra101.mppcg.node.predicate.QuantifierPredicate
import dobra101.mppcg.node.substitution.ParallelSubstitution

interface BEnvironment {

    /* Expression */
    fun Function.renderSelf(): RenderResult
    fun CallFunctionExpression.renderSelf(): RenderResult
    fun Couple.renderSelf(): RenderResult
    fun DomainFunctionExpression.renderSelf(): RenderResult
    fun DomainRestrictionExpression.renderSelf(): RenderResult
    fun DomainSubtractionExpression.renderSelf(): RenderResult
    fun ImageFunctionExpression.renderSelf(): RenderResult
    fun IntersectionCollectionExpression.renderSelf(): RenderResult
    fun RangeFunctionExpression.renderSelf(): RenderResult
    fun RangeRestrictionExpression.renderSelf(): RenderResult
    fun RangeSubtractionExpression.renderSelf(): RenderResult
    fun ReverseFunctionExpression.renderSelf(): RenderResult
    fun SubtractionCollectionExpression.renderSelf(): RenderResult
    fun UnionCollectionExpression.renderSelf(): RenderResult

    /* Predicate */
    fun Invariant.renderSelf(): RenderResult
    fun QuantifierPredicate.renderSelf(): RenderResult

    /* Substitution */
    fun Initialization.renderSelf(): RenderResult
    fun ParallelSubstitution.renderSelf(): RenderResult
    fun Precondition.renderSelf(): RenderResult
    fun Select.renderSelf(): RenderResult

    /* Machine */
    fun Machine.renderSelf(): RenderResult
    fun Operation.renderSelf(): RenderResult
    fun Transition.renderSelf(): RenderResult

}