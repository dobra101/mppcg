package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function

interface BEnvironment {

    /* Expression */
    fun Function.renderSelf(): RenderResult

    /* Predicate */
    fun Invariant.renderSelf(): RenderResult

    /* Substitution */
    fun Initialization.renderSelf(): RenderResult
    fun Precondition.renderSelf(): RenderResult
    fun Select.renderSelf(): RenderResult

    /* Machine */
    fun Machine.renderSelf(): RenderResult
    fun Operation.renderSelf(): RenderResult
    fun Transition.renderSelf(): RenderResult

}