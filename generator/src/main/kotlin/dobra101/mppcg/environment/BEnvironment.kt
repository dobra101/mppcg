package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.BinaryCollectionExpression
import dobra101.mppcg.node.collection.UnaryCollectionExpression
import dobra101.mppcg.node.predicate.QuantifierPredicate
import dobra101.mppcg.node.substitution.ParallelSubstitution

interface BEnvironment {

    /* Expression */
    fun Function.renderSelf(): RenderResult
    fun BinaryCollectionExpression.renderSelf(): RenderResult
    fun BinaryFunctionExpression.renderSelf(): RenderResult
    fun CallFunctionExpression.renderSelf(): RenderResult
    fun ComprehensionSet.renderSelf(): RenderResult
    fun ConcreteIdentifierExpression.renderSelf(): RenderResult
    fun Couple.renderSelf(): RenderResult
    fun InfiniteSet.renderSelf(): RenderResult
    fun UnaryCollectionExpression.renderSelf(): RenderResult
    fun UnaryFunctionExpression.renderSelf(): RenderResult

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