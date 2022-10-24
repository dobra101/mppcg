package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.expression.BinaryExpression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.expression.IntervalExpression
import dobra101.mppcg.node.expression.ValueExpression
import dobra101.mppcg.node.predicate.BinaryPredicate
import dobra101.mppcg.node.predicate.LogicPredicate
import dobra101.mppcg.node.substitution.AssignSubstitution

class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    /* ---------- EXPRESSIONS ---------- */
    override fun BinaryExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun ValueExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }


    /* ---------- PREDICATES ---------- */
    override fun BinaryPredicate.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun LogicPredicate.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }


    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Invariant.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Initialization.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Precondition.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Select.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Machine.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Operation.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Transition.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }


    override fun type2String(type: Type): String {
        TODO("Not yet implemented")
    }
}