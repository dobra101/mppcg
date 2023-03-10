package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import java.util.*

// TODO: add constructor for generated classes
class JavaOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/java"
    override val fileExtension = "java"

    private val optimizer = JavaOptimizer(this)

    private var codeRepresentation: Any? = null

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousSetCollectionNode.renderSelf(): RenderResult {
        val map = mapOf(
            "elements" to elements.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to operator2String(operator)
        )

        return RenderResult(renderTemplate(map))
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name.capitalize(),
            "elements" to elements.map { it.name.uppercase() } // TODO: refactor
        )

        return RenderResult(renderTemplate(map))
    }

    override fun EnumEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name.uppercase(),
            "enum" to enum.capitalize()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left.render(),
            "right" to right.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun SetCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun SetEntry.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: Same for Java and Prolog
    override fun ValueExpression.renderSelf(): RenderResult {
        val map = if (type is TypeBoolean && (type as TypeBoolean).value != null) {
            mapOf("value" to ((type as TypeBoolean).value == BooleanValue.TRUE).toString())
        } else {
            mapOf("value" to value)
        }

        return RenderResult(renderTemplate(map))
    }


    /* ---------- PREDICATES ---------- */
    // TODO: refactor
    override fun BinaryPredicate.renderSelf(): RenderResult {
        // Predicate is a type check
        if (right is EnumCollectionNode && operator == BinaryPredicateOperator.MEMBER) {
            // TODO: neglect if types already match
            val map = mapOf(
                "lhs" to left.render(),
                "operator" to "instanceof",
                "rhs" to (right as EnumCollectionNode).name.capitalize()
            )
            return RenderResult(renderTemplate(map))
        }

        if ((right is AnonymousSetCollectionNode || right is IntervalExpression)
            && operator == BinaryPredicateOperator.MEMBER) {
            val map = mapOf(
                "entry" to left.render(),
                "set" to right.render()
            )
            return RenderResult(renderTemplate("binaryPredicateMember", map))
        }

        if ((right is InfiniteSet || operator == BinaryPredicateOperator.MEMBER)) {
            val lhs = left.render()
            val map = mapOf(
                "lhs" to lhs,
                "rhs" to "(${type2String(right.type)})${lhs.rendered}",
                "operator" to operator2String(BinaryPredicateOperator.EQUAL)
            )
            return RenderResult(renderTemplate(map))
        }

        val map = mapOf(
            "lhs" to left.render(),
            "operator" to operator2String(operator),
            "rhs" to right.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun BinaryLogicPredicate.renderSelf(): RenderResult {
        val lhs = if (left is BinaryLogicPredicate) "(${left.render().rendered})" else left.render().rendered
        val rhs = if (right is BinaryLogicPredicate) "(${right.render().rendered})" else right.render().rendered

        val map = mapOf(
            "lhs" to lhs,
            "operator" to operator2String(operator),
            "rhs" to rhs
        )

        return RenderResult(renderTemplate(map))
    }

    override fun UnaryLogicPredicate.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        if (optimize) optimizer.renderOptimized(this)?.let { return it }

        val map = mapOf(
            "identifier" to (lhs as IdentifierExpression).render(),
            "rhs" to rhs.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun DeclarationSubstitution.renderSelf(): RenderResult {
        // TODO: optimize?
        val map = mapOf(
            "type" to type2String(type),
            "lhs" to assignment.lhs.render(),
            "rhs" to assignment.rhs.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun ElseIfSubstitution.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun IfSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then.render(),
            "elseIf" to elseIf.render(),
            "elseSubstitution" to elseSubstitution?.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun SequenceSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun WhileSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "condition" to condition.render(),
            "body" to body.render()
        )

        return RenderResult(renderTemplate(map))
    }

    /* ---------- CLASS BLOCK ---------- */
    override fun ClassVariables.renderSelf(): RenderResult {
        val declarations = variables.filterIsInstance<IdentifierExpression>()
            .map {
                val map = mapOf(
                    "type" to type2String(it.type),
                    "lhs" to it.name,
                    "classVar" to true
                )
                renderTemplate("declarationSubstitution", map)
            }

        val map = mapOf(
            "variables" to declarations
        )

        return RenderResult(renderTemplate(map))
    }

    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left,
            "right" to right
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left.render(),
            "right" to right.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinarySequenceExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "expression" to expression.render(),
            "parameters" to parameters.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ComprehensionSet.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun ConcreteIdentifierExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "value" to value.render(),
            "type" to type2String(type)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun Couple.renderSelf(): RenderResult {
        val map = mapOf(
            "from" to from.render(),
            "to" to to.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun InfiniteSet.renderSelf(): RenderResult {
        // HINT: n : INTEGER -> n.type = INTEGER
        return RenderResult(type2String(type)) // TODO: refactor?
    }

    override fun LambdaExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "identifier" to identifiers.render(),
            "predicate" to predicate.render(),
            "expression" to expression.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun Sequence.renderSelf(): RenderResult {
        val map = mapOf(
            "elements" to elements.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnarySequenceExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "sequence" to sequence.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnaryCollectionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "collection" to collection.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnaryExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        val renderedPredicates = List(predicates.size) { idx ->
            val map = mapOf(
                "body" to predicates[idx].render(),
                "idx" to idx
            )
            renderTemplate(map)
        }

        val map = mapOf("list" to renderedPredicates)

        return RenderResult(renderTemplate("invariants", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Initialization.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render(),
            "name" to (codeRepresentation as Machine).name
        )

        return RenderResult(renderTemplate(map))
    }

    override fun ParallelSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Precondition.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun Select.renderSelf(): RenderResult {
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then?.render(),
            "elseSubstitution" to elseSubstitution?.render(),
            "when" to whenSubstitution.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Machine.renderSelf(): RenderResult {
        codeRepresentation = this

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
//            "properties" to properties?.render(),
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization?.render(),
            "invariant" to invariant.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "transitions" to transitions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Operation.renderSelf(): RenderResult {
        if (optimize) optimizer.renderOptimized(this)?.let { return it }

        val bodyUsed = (body as? Precondition)?.substitution ?: body

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "returnValues" to returnValues.render(),
            "body" to bodyUsed?.render(),
            "type" to type2String(type)
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Transition.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "body" to body.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun type2String(type: Type?): String {
        if (type == null) throw InvalidTypeException("Null as type found")
        return when (type) {
            is TypeAnonymousCollection -> "anonymous type"
            is TypeBoolean -> "boolean"
            is TypeCollection -> if (type.type == CollectionType.Enum) type.name.capitalize() else type.name
            is TypeInteger -> "int"
            is TypeNatural -> "int"
            is TypeReal -> "double"
            is TypeString -> "String"
            is TypeVoid -> "void"
            is TypeFunction -> "BRelation"
            is TypeSet -> "Set<${type2String(type.type)}>"
            else -> throw UnknownTypeException(type::class.simpleName!!)
        }
    }

    override fun operator2String(operator: BinaryPredicateOperator): String {
        return when (operator) {
            BinaryPredicateOperator.GREATER -> ">"
            BinaryPredicateOperator.GREATER_EQUAL -> ">="
            BinaryPredicateOperator.LESS -> "<"
            BinaryPredicateOperator.LESS_EQUAL -> "<="
            BinaryPredicateOperator.EQUAL -> "=="
            BinaryPredicateOperator.NOT_EQUAL -> "!="
            BinaryPredicateOperator.MEMBER -> "instanceof"
            BinaryPredicateOperator.NOT_MEMBER -> "notmember(java)"
            BinaryPredicateOperator.SUBSET -> "subset(java)"
        }
    }

    private fun String.capitalize(): String {
        return replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }
    }
}