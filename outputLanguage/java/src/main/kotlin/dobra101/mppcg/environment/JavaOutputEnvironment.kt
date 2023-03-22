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
import kotlin.math.exp

// TODO: add constructor for generated classes
class JavaOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/java"
    override val fileExtension = "java"

    private val optimizer = JavaOptimizer(this)

    private var codeRepresentation: Any? = null

    private var currentOperation: Operation? = null // HINT: only for B
    private var inInvariant: Boolean = false // HINT: only for B

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousSetCollectionNode.renderSelf(): RenderResult {
        fun renderAnonymousSetAsRelation(set: AnonymousSetCollectionNode): RenderResult {
            val map = mutableMapOf(
                "elements" to set.elements.render()
            )

            return RenderResult(renderTemplate("anonymousSetAsRelation", map))
        }

        if (elements.isNotEmpty() && elements.first() is Couple) {
            return renderAnonymousSetAsRelation(this)
        }

        val map = mutableMapOf(
            "elements" to elements.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to if (left.type is TypeNumber) operator2String(operator) else customOperator2String(operator),
            "customOperator" to (left.type !is TypeNumber)
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
        // TODO: central keyword handler
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
        if (operator != BinaryPredicateOperator.MEMBER) {
            val map = mapOf(
                "lhs" to left.render(),
                "operator" to operator2String(operator),
                "rhs" to right.render(),
                "isMethodCall" to (operator == BinaryPredicateOperator.SUBSET)
            )

            return RenderResult(renderTemplate(map))
        }

        if (right is EnumCollectionNode) {
            // TODO: neglect if types already match
            val map = mapOf(
                "lhs" to left.render(),
                "operator" to "instanceof",
                "rhs" to (right as EnumCollectionNode).name.capitalize()
            )
            return RenderResult(renderTemplate(map))
        }

        if (right is AnonymousSetCollectionNode || right is IntervalExpression ||
            (right is IdentifierExpression && right.type is TypeSet)
        ) {
            val map = mapOf(
                "entry" to left.render(),
                "set" to right.render()
            )
            return RenderResult(renderTemplate("binaryPredicateMember", map))
        }

        // type checks in invariants can be ignored in java
        if (inInvariant) {
            return RenderResult("")
        }
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryLogicPredicate.renderSelf(): RenderResult {
        val lhs = if (left is BinaryLogicPredicate) "(${left.render().rendered})" else left.render().rendered
        val rhs = if (right is BinaryLogicPredicate) "(${right.render().rendered})" else right.render().rendered

        val map = mapOf(
            "lhs" to lhs,
            "operator" to operator2String(operator),
            "rhs" to rhs,
            "parenthesis" to (operator != LogicPredicateOperator.AND),
            "implication" to (operator == LogicPredicateOperator.IMPLIES) // TODO: refactor
        )

        return RenderResult(renderTemplate(map))
    }

    override fun UnaryLogicPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "predicate" to predicate.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ValuePredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "value" to if (type is TypeBoolean) ((type as TypeBoolean).value == BooleanValue.TRUE).toString() else value
        )
        return RenderResult(renderTemplate(map))
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        fun renderAnonymousSetAsRelation(set: AnonymousSetCollectionNode): RenderResult {
            val map = mutableMapOf(
                "elements" to set.elements.render()
            )

            return RenderResult(renderTemplate("anonymousSetAsRelation", map))
        }

        if (left is CallFunctionExpression) {
            val parameters = (left as CallFunctionExpression).parameters.toMutableList()
            parameters.add(right)
            val newLeft = CallFunctionExpression(
                (left as CallFunctionExpression).expression,
                parameters,
                (left as CallFunctionExpression).operator
            )

            // TODO: refactor
            val rendered = newLeft.renderSelf()
            return RenderResult(rendered.rendered + ";", rendered.info)
        }

        if (optimize) optimizer.renderOptimized(this)?.let { return it }

        val rhs = if (right is AnonymousSetCollectionNode && left.type is TypeFunction) {
            renderAnonymousSetAsRelation(right as AnonymousSetCollectionNode)
        } else {
            right.render()
        }

        val map = mapOf(
            "identifier" to left.render(),
            "rhs" to rhs
        )

        return RenderResult(renderTemplate(map))
    }

    override fun DeclarationSubstitution.renderSelf(): RenderResult {
        // TODO: optimize?
        val map = mapOf(
            "type" to type2String(type),
            "lhs" to assignment.left.render(),
            "rhs" to assignment.right.render()
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
        val map = if (operator == BinaryCollectionOperator.CONCAT && right is AnonymousSetCollectionNode) {
            mapOf(
                "lhs" to left.render(),
                "rhs" to (right as AnonymousSetCollectionNode).elements.render(),
                "operator" to operator2String(operator),
                "rhsIsList" to true
            )
        } else {
            mapOf(
                "lhs" to left.render(),
                "rhs" to right.render(),
                "operator" to operator2String(operator)
            )
        }
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left.render(),
            "right" to right.render(),
            "operator" to operator2String(operator),
            "swap" to (operator == BinaryFunctionOperator.DOMAIN_SUBTRACTION || operator == BinaryFunctionOperator.DOMAIN_RESTRICTION)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinarySequenceExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left.render(),
            "right" to right.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        if (expression is BinaryCollectionExpression && (
                    (expression as BinaryCollectionExpression).operator == BinaryCollectionOperator.PRJ1 ||
                            (expression as BinaryCollectionExpression).operator == BinaryCollectionOperator.PRJ2)
        ) {
            val map = mapOf(
                "expression" to parameters.render(),
                "operator" to operator2String((expression as BinaryCollectionExpression).operator)
            )
            return RenderResult(renderTemplate(map))
        }

        val map = mapOf(
            "expression" to expression.render(),
            "parameters" to parameters.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ComprehensionSet.renderSelf(): RenderResult {
        fun Predicate.memberAndComprehensionSetIdentifier(identifiers: List<Expression>): Boolean {
            if (this !is BinaryPredicate) return false
            if (operator != BinaryPredicateOperator.MEMBER) return false
            return identifiers.contains(left)
        }

        val predicateList = predicates.asList()
        val condition: MutableList<Predicate> = mutableListOf()
        val supplier: MutableList<Map<String, String>> = mutableListOf()
        predicateList.forEach {
            if (it.memberAndComprehensionSetIdentifier(identifiers)) {
                val map = mapOf(
                    (it as BinaryPredicate).left.render().rendered to it.right.render().rendered
                )
                supplier.add(map)
            } else {
                condition.add(it)
            }
        }

        val map = mapOf(
            "identifiers" to identifiers.render(),
            "supplier" to supplier,
            "predicates" to condition.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ConcreteIdentifierExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "value" to value.render(),
            "type" to type2String(type),
            "declare" to (currentOperation == null)
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
        val map = mapOf(
            "value" to value.render(),
            "operator" to operator2String(operator),
            "parenthesis" to (operator != UnaryExpressionOperator.MINUS)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "expression" to expression.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        inInvariant = true
        val renderedPredicates = List(predicates.size) { idx ->
            val body = predicates[idx].render()
            val ignoreTypeCheck = body.info.isEmpty()
            val map = mapOf(
                "body" to body,
                "idx" to idx,
                "ignoreTypeCheck" to ignoreTypeCheck
            )
            renderTemplate(map)
        }

        val map = mapOf("list" to renderedPredicates)
        inInvariant = false

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

    // TODO: use temp variables
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
        currentOperation = this
        if (optimize) optimizer.renderOptimized(this)?.let { return it }

        val bodyUsed = (body as? Precondition)?.substitution ?: body

        val typedParameters = parameters.filterIsInstance<IdentifierExpression>()
            .map {
                renderTemplate(
                    "parameterExpression",
                    mapOf(
                        "name" to it.name,
                        "type" to type2String(it.type)
                    )
                )
            }

        val map = mapOf(
            "name" to name,
            "parameters" to typedParameters,
            "returnValues" to returnValues.render(),
            "body" to bodyUsed?.render(),
            "type" to type2String(type)
        )

        currentOperation = null
        return RenderResult(renderTemplate(map))
    }

    override fun Transition.renderSelf(): RenderResult {
        val typedParameters = parameters.filterIsInstance<IdentifierExpression>()
            .map {
                renderTemplate(
                    "parameterExpression",
                    mapOf(
                        "name" to it.name,
                        "type" to type2String(it.type)
                    )
                )
            }

        val map = mapOf(
            "name" to name,
            "parameters" to typedParameters,
            "body" to body.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun type2String(type: Type?): String {
        if (type == null) throw InvalidTypeException("Null as type found")
        return when (type) {
            is TypeAnonymousCollection -> "anonymous type"
            is TypeBoolean -> "Boolean"
            is TypeCollection -> if (type.type == CollectionType.Enum) type.name.capitalize() else type.name
            is TypeInteger -> "Integer"
            is TypeNatural -> "Integer"
            is TypeReal -> "Double"
            is TypeString -> "String"
            is TypeVoid -> "void"
            is TypeFunction -> {
                if (type.from == null || type.to == null) {
                    "BRelation<>"
                } else {
                    "BRelation<${nullableType2String(type.from!!)}, ${nullableType2String(type.to!!)}>"
                }
            }

            is TypeSet -> "BSet<${nullableType2String(type.type)}>"
            is TypeSequence -> "BSequence<${nullableType2String(type.type!!)}>"
            is TypeCouple -> {
                if (type.from == null || type.to == null) {
                    "BCouple<>"
                } else {
                    "BCouple<${nullableType2String(type.from!!)}, ${nullableType2String(type.to!!)}>"
                }
            }

            else -> throw UnknownTypeException(type::class.simpleName!!)
        }
    }

    // TODO: still needed?
    private fun nullableType2String(type: Type): String {
        return when (type) {
            is TypeBoolean -> "Boolean"
            is TypeInteger -> "Integer"
            is TypeNatural -> "Integer"
            is TypeReal -> "Double"
            is TypeString -> "String"
            is TypeFunction -> {
                if (type.from == null || type.to == null) {
                    "BRelation<>"
                } else {
                    "BRelation<${nullableType2String(type.from!!)}, ${nullableType2String(type.to!!)}>"
                }
            }

            is TypeSet -> "BSet<${nullableType2String(type.type)}>"
            is TypeSequence -> "BSequence<${nullableType2String(type.type!!)}>"
            is TypeCouple -> {
                if (type.from == null || type.to == null) {
                    "BCouple<>"
                } else {
                    "BCouple<${nullableType2String(type.from!!)}, ${nullableType2String(type.to!!)}>"
                }
            }

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
            BinaryPredicateOperator.SUBSET -> "containsAll"
        }
    }

    override fun operator2String(operator: BinaryCollectionOperator): String {
        return when (operator) {
            BinaryCollectionOperator.INTERSECTION -> "intersection"
            BinaryCollectionOperator.SUBTRACTION -> "subtraction"
            BinaryCollectionOperator.UNION -> "union"
            BinaryCollectionOperator.CONCAT -> "concat"
            BinaryCollectionOperator.PRJ1 -> "prj1"
            BinaryCollectionOperator.PRJ2 -> "prj2"
        }
    }


    private fun customOperator2String(operator: BinaryExpressionOperator): String {
        return when (operator) {
            BinaryExpressionOperator.ADD -> "add"
            BinaryExpressionOperator.MINUS -> "minus"
            BinaryExpressionOperator.MULT -> "mult"
            BinaryExpressionOperator.DIV -> "div"
            BinaryExpressionOperator.MOD -> "mod"
        }
    }

    private fun String.capitalize(): String {
        return replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }
    }
}