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

class JavaOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/java"
    override val fileExtension = "java"

    /* ---------- EXPRESSIONS ---------- */
    override fun BinaryExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render().rendered,
            "rhs" to right.render().rendered,
            "operator" to operator2String(operator)
        )

        return RenderResult(stRender("binaryExpression", map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(stRender("identifierExpression", map))
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun ValueExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "value" to value
        )

        return RenderResult(stRender("valueExpression", map))
    }


    /* ---------- PREDICATES ---------- */
    // HINT: SAME AS LOGIC PREDICATE
    override fun BinaryPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render().rendered,
            "operator" to operator2String(operator),
            "rhs" to right.render().rendered
        )

        return RenderResult(stRender("binaryPredicate", map))
    }

    // HINT: SAME AS BINARY PREDICATE
    override fun LogicPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render().rendered,
            "operator" to operator2String(operator),
            "rhs" to right.render().rendered
        )

        return RenderResult(stRender("logicPredicate", map))
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "identifier" to (lhs[0] as IdentifierExpression).render().rendered, // TODO: when more than one entry?
            "rhs" to rhs.render()
        )

        return RenderResult(stRender("assignSubstitution", map))
    }


    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Invariant.renderSelf(): RenderResult {
        val map = mapOf(
            "body" to predicate.render().rendered
        )

        return RenderResult(stRender("invariant", map))
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
        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to (constraints?.render()?.rendered ?: ""),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
            "properties" to (properties?.render()?.rendered ?: ""),
            "definitions" to (definitions?.render()?.rendered ?: ""),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
//            "initialization" to (initialization?.render()?.rendered ?: ""),
            "invariant" to (invariant?.render()?.rendered ?: ""),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "transitions" to transitions.render()
        )

        return RenderResult(stRender("machine", map))
    }

    override fun Operation.renderSelf(): RenderResult {
        val bodyUsed = (body as? Precondition)?.substitution ?: body

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
//            "returnValues" to returnValues.render(), // TODO: use returnValues
            "body" to bodyUsed.render().rendered
        )

        return RenderResult(stRender("operation", map))
    }

    override fun Transition.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "body" to body.render().rendered
        )

        return RenderResult(stRender("transition", map))
    }


    override fun type2String(type: Type): String {
        TODO("Not yet implemented")
    }
}