package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.BinaryExpression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.expression.IntervalExpression
import dobra101.mppcg.node.expression.ValueExpression
import dobra101.mppcg.node.predicate.BinaryPredicate
import dobra101.mppcg.node.predicate.LogicPredicate
import dobra101.mppcg.node.predicate.QuantifierPredicate
import dobra101.mppcg.node.substitution.AssignSubstitution
import dobra101.mppcg.node.substitution.ParallelSubstitution

// TODO: add constructor for generated classes
class JavaOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/java"
    override val fileExtension = "java"

    private val optimizer = JavaOptimizer(this)

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousSetCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to operator2String(operator)
        )

        return RenderResult(stRender("binaryExpression", map))
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun EnumEntry.renderSelf(): RenderResult {
        TODO("Not yet implemented")
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

    override fun SetCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun SetEntry.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: Same for Java and Prolog
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
            "lhs" to left.render(),
            "operator" to operator2String(operator),
            "rhs" to right.render()
        )

        return RenderResult(stRender("binaryPredicate", map))
    }

    // HINT: SAME AS BINARY PREDICATE
    override fun LogicPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "operator" to operator2String(operator),
            "rhs" to right.render()
        )

        return RenderResult(stRender("logicPredicate", map))
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        if (optimize) optimizer.renderOptimized(this)?.let { return it }

        val map = mapOf(
            "identifier" to (lhs[0] as IdentifierExpression).render(), // TODO: when more than one entry?
            "rhs" to rhs.render()
        )

        return RenderResult(stRender("assignSubstitution", map))
    }

    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Couple.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        val map = mapOf(
            "body" to predicate.render()
        )

        return RenderResult(stRender("invariant", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Initialization.renderSelf(): RenderResult {
        val subs = substitutions.map {
            // TODO: convert to declarationSubstitutionNode while converting?
            if (it is AssignSubstitution) {
                val map = mapOf(
                    "type" to type2String(it.lhs[0].type), // TODO: when more than one identifier?
                    "lhs" to it.lhs[0].render(), // TODO: when more than one identifier?
                    "rhs" to it.rhs.render()
                )
                stRender("declarationSubstitution", map)
            } else {
                it.render().rendered
            }
        }

        val map = mapOf(
            "substitutions" to subs
        )

        return RenderResult(stRender("initialization", map))
    }


    override fun ParallelSubstitution.renderSelf(): RenderResult {
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
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
            "properties" to properties?.render(),
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization?.render(),
            "invariant" to invariant?.render(),
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
            "body" to bodyUsed.render()
        )

        return RenderResult(stRender("operation", map))
    }

    override fun Transition.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "body" to body.render()
        )

        return RenderResult(stRender("transition", map))
    }


    override fun type2String(type: Type): String {
        return when (type) {
            is TypeReal -> "double"
            is TypeInteger -> "int"
            is TypeString -> "String"
            is TypeVoid -> "void"
            is TypeCollection -> type.name
            else -> throw UnknownTypeException(type::class.simpleName!!)
        }
    }
}