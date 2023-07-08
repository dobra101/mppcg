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

/**
 * This is the [OutputLanguageEnvironment] for Java.
 *
 * Here, the ```renderSelf```-method is implemented for each node.
 *
 * Collapse all methods for a better understanding of what this class does.
 */
class JavaOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/java"
    override val fileExtension = "java"

    private var codeRepresentation: Program? = null

    private var currentOperation: Operation? = null // HINT: only for B
    private var inInvariant: Boolean = false // HINT: only for B

    private var memberAsIterator: Boolean = false // HINT: only for B
    private var equalsIsAssignment: Boolean = false // HINT: only for B
    private var iteratorCount: Int = 0

    private val MPPCG_MAX_INT: String = "RunConfig.maxInt"
    private val MPPCG_MIN_INT: String = "RunConfig.minInt"


    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousCollectionNode.renderSelf(): RenderResult {
        val map = mutableMapOf(
            "elements" to elements.render(),
            "type" to type.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to if (left.type is TypeNumber) operator.render() else operator.renderCustomMethod(),
            "customOperator" to (left.type !is TypeNumber)
        )

        return RenderResult(renderTemplate(map))
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name.capitalize(),
            "elements" to elements.map { it.name },
            "isParameter" to isParameter
        )

        return RenderResult(renderTemplate(map))
    }

    override fun EnumEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "enum" to enum.capitalize()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        fun isConcrete(): Boolean {
            val mch = codeRepresentation as Machine

            return mch.concreteVariables.union(mch.concreteConstants).union(mch.constants)
                .filter { it is IdentifierExpression || it is ConcreteIdentifierExpression }
                .find {
                    (it is IdentifierExpression && it.name == name) || ((it as ConcreteIdentifierExpression).name == "c_$name")
                } != null
        }

        val enumNode = (codeRepresentation as Machine).sets.find { it.elements.find { e -> e.name == name } != null }
        val prefix = when {
            enumNode != null -> enumNode.name.capitalize() + "."
            isConcrete() -> "c_"
            else -> ""
        }
        val map = mapOf(
            "name" to "$prefix$name"
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

    override fun ValueExpression.renderSelf(): RenderResult {
        val map = mapOf("value" to value)
        return RenderResult(renderTemplate(map))
    }


    /* ---------- PREDICATES ---------- */
    override fun BinaryPredicate.renderSelf(): RenderResult {
        fun mapType2String(type: FunctionMapType): String {
            return when (type) {
                FunctionMapType.FUNCTION -> "isFunction()"
                FunctionMapType.INJECTION -> "isInjection()"
                FunctionMapType.SURJECTION -> "isSurjection()"
                FunctionMapType.BIJECTION -> "isBijection()"
            }
        }

        fun functionType2String(type: FunctionType): String {
            return when (type) {
                FunctionType.TOTAL -> "isTotal(${(right as Function).left.render().rendered})"
                FunctionType.PARTIAL -> "isPartial()"
            }
        }

        if (equalsIsAssignment && operator == BinaryPredicateOperator.EQUAL) {
            if (right !is ComprehensionSet) {
                val map = mapOf(
                    "identifier" to left.render(),
                    "rhs" to right.render()
                )
                return RenderResult(renderTemplate("assignSubstitution", map))
            }

            val map = mapOf(
                "type" to left.type.render(),
                "identifier" to left.render(),
                "body" to right.render(),
                "ic" to iteratorCount
            )
            return RenderResult(renderTemplate("delayedAssignSubstitution", map))
        }

        if (operator != BinaryPredicateOperator.MEMBER) {
            val map = mapOf(
                "lhs" to left.render(),
                "operator" to operator.render(),
                "rhs" to right.render(),
                "isMethodCall" to (operator == BinaryPredicateOperator.SUBSET
                        || operator == BinaryPredicateOperator.NOT_MEMBER
                        || operator == BinaryPredicateOperator.STRICT_SUBSET)
            )

            return RenderResult(renderTemplate(map))
        }

        if (right is Function) {
            val map = mapOf(
                "left" to left.render(),
                "from" to (right as Function).left.render(),
                "to" to (right as Function).right.render(),
                "functionType" to functionType2String((right as Function).functionType),
                "mapType" to mapType2String((right as Function).mapType)
            )
            return RenderResult(renderTemplate("functionTypeCheck", map))
        }

        if (memberAsIterator) {
            val map = mapOf(
                "type" to left.type.render(),
                "name" to left.render(),
                "collection" to right.render()
            )
            return RenderResult(renderTemplate("iteratorConstruct", map))
        }

        if (right is EnumCollectionNode) {
            val map = mapOf(
                "lhs" to left.render(),
                "operator" to "instanceof",
                "rhs" to (right as EnumCollectionNode).name.capitalize()
            )
            return RenderResult(renderTemplate(map))
        }

        if (right.type is TypeSet) {
            if (right is AnonymousCollectionNode && (right as AnonymousCollectionNode).elements.isEmpty()) {
                // type check
                val map = mapOf(
                    "lhs" to left.render(),
                    "rhs" to (right as AnonymousCollectionNode).collectionType.render(),
                    "operator" to operator.render()
                )
                return RenderResult(renderTemplate(map))
            }
            val map = mapOf(
                "entry" to left.render(),
                "set" to right.render()
            )
            return RenderResult(renderTemplate("binaryPredicateMember", map))
        }

        val map = mapOf(
            "lhs" to left.render(),
            "rhs" to right.render(),
            "operator" to operator.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryLogicPredicate.renderSelf(): RenderResult {
        val lhs = if (left is BinaryLogicPredicate) "(${left.render().rendered})" else left.render().rendered
        val rhs = if (right is BinaryLogicPredicate) "(${right.render().rendered})" else right.render().rendered

        val map = mapOf(
            "lhs" to lhs,
            "operator" to operator.render(),
            "rhs" to rhs,
            "parenthesis" to (operator != LogicPredicateOperator.AND),
            "implication" to (operator == LogicPredicateOperator.IMPLIES)
        )

        return RenderResult(renderTemplate(map))
    }

    override fun UnaryLogicPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "predicate" to predicate.render(),
            "operator" to operator.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ValuePredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "value" to value
        )
        return RenderResult(renderTemplate(map))
    }


    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        fun canBeFurtherOptimized(expr: Expression, operator: BinaryExpressionOperator): Boolean {
            val op = operator == BinaryExpressionOperator.ADD || operator == BinaryExpressionOperator.MINUS
            return op && expr is ValueExpression && expr.value.toIntOrNull() == 1
        }
        fun assignSelf(
            left: Expression,
            right: Expression,
            operator: BinaryExpressionOperator,
            target: Expression
        ): RenderResult? {
            if (target is IdentifierExpression && left == target && left.type is TypeNumber) {
                val map = if (canBeFurtherOptimized(right, operator)) {
                    mapOf(
                        "identifier" to target.name,
                        "operator" to if (operator == BinaryExpressionOperator.ADD) "++" else "--"
                    )
                } else {
                    mapOf(
                        "identifier" to target.name,
                        "operator" to operator.render(),
                        "rhs" to right.render()
                    )
                }
                return RenderResult(renderTemplate("optimizedAssignSubstitution", map))
            }
            return null
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

        if (right is BinaryExpression) {
            val binaryExpr = right as BinaryExpression

            // e.g. a = a + 1 or a = 1 + a
            val result = assignSelf(binaryExpr.left, binaryExpr.right, binaryExpr.operator, left)
                ?: assignSelf(binaryExpr.right, binaryExpr.left, binaryExpr.operator, left)
            result?.let { return it }
        }

        val rhs = right.render()

        val map = mapOf(
            "identifier" to left.render(),
            "rhs" to rhs
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
                    "type" to it.type.render(),
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
            "left" to left.render(),
            "right" to right.render(),
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        val map = if (operator == BinaryCollectionOperator.CONCAT && right is AnonymousCollectionNode) {
            mapOf(
                "lhs" to left.render(),
                "rhs" to (right as AnonymousCollectionNode).elements.render(),
                "operator" to operator.render(),
                "rhsIsList" to true
            )
        } else {
            mapOf(
                "lhs" to left.render(),
                "rhs" to right.render(),
                "operator" to operator.render()
            )
        }
        return RenderResult(renderTemplate(map))
    }

    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left.render(),
            "right" to right.render(),
            "operator" to operator.render(),
            "swap" to (operator == BinaryFunctionOperator.DOMAIN_SUBTRACTION || operator == BinaryFunctionOperator.DOMAIN_RESTRICTION)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun BinarySequenceExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "left" to left.render(),
            "right" to right.render(),
            "operator" to operator.render()
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
                "operator" to operator.render()
            )
            return RenderResult(renderTemplate(map))
        }

        val map = mapOf(
            "expression" to expression.render(),
            "parameters" to parameters.render(),
            "operator" to operator.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ComprehensionSet.renderSelf(): RenderResult {
        iteratorCount++
        fun Predicate.memberAndComprehensionSetIdentifier(identifiers: List<Expression>): Boolean {
            if (this !is BinaryPredicate) return false
            if (operator != BinaryPredicateOperator.MEMBER) return false
            return identifiers.contains(left)
        }

        val predicateList = predicates.asList()
        val condition: MutableList<Predicate> = mutableListOf()
        val supplier: MutableMap<String, Pair<String, String>> = mutableMapOf()
        predicateList.forEach {
            if (it.memberAndComprehensionSetIdentifier(identifiers)) {
                val id = (it as BinaryPredicate).left
                val key = id.render().rendered
                if (!supplier.containsKey(key)) {
                    supplier[key] = Pair(id.type.render(), it.right.render().rendered)
                }
            } else {
                condition.add(it)
            }
        }

        val iterators = supplier.map {
            val itMap = mapOf(
                "type" to it.value.first,
                "name" to it.key,
                "collection" to it.value.second
            )
            RenderResult(renderTemplate("iteratorConstruct", itMap)).rendered
        }

        val map = mapOf(
            "identifiers" to identifiers.render(),
            "iterators" to iterators,
            "predicates" to condition.render(),
            "ic" to iteratorCount
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ConcreteIdentifierExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name,
            "value" to if (value is LambdaExpression || value is ComprehensionSet) null else value.render(),
            "type" to type.render(),
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

    override fun GeneralSumOrProductExpression.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    override fun InfiniteSet.renderSelf(): RenderResult {
        fun createInterval(from: String, to: String): RenderResult {
            val map = mapOf("left" to from, "right" to to)
            return RenderResult(renderTemplate("intervalExpression", map))
        }

        val interval = when (setType) {
            MPPCG_Int -> createInterval(MPPCG_MIN_INT, MPPCG_MAX_INT)
            MPPCG_Nat -> createInterval("0", MPPCG_MAX_INT)
            MPPCG_Nat1 -> createInterval("1", MPPCG_MAX_INT)
            else -> null
        }

        val map = mapOf(
            "type" to type.render(),
            "interval" to interval
        )
        return RenderResult(renderTemplate(map))
    }

    override fun LambdaExpression.renderSelf(): RenderResult {
        val predicateRendered =
            if (predicate is BinaryPredicate && (predicate as BinaryPredicate).right is IntervalExpression) {
                (predicate as BinaryPredicate).right.render()
            } else {
                predicate.render()
            }
        val map = mapOf(
            "identifier" to identifiers.render(),
            "type" to type.render(),
            "predicate" to predicateRendered,
            "expression" to expression.render(),
            "exprType" to expression.type.render(),
            "identifierType" to identifiers.map { it.type.render() }
        )
        return RenderResult(renderTemplate(map))
    }

    override fun Sequence.renderSelf(): RenderResult {
        val map = mapOf(
            "elements" to elements.render(),
            "type" to type.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnarySequenceExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "sequence" to sequence.render(),
            "operator" to operator.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnaryCollectionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "collection" to collection.render(),
            "operator" to operator.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnaryExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "value" to value.render(),
            "operator" to operator.render(),
            "parenthesis" to (operator != UnaryExpressionOperator.MINUS)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        val map = mapOf(
            "expression" to expression.render(),
            "operator" to operator.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun Invariant.renderSelf(): RenderResult {
        inInvariant = true
        val renderedPredicates = List(predicates.size) { idx ->
            iteratorCount = 0
            val body = predicates[idx].render()
            val map = mapOf(
                "ic" to 1,
                "body" to body,
                "idx" to idx,
                "inline" to (body.rendered.count { it == '\n' } == 0)
            )
            renderTemplate(map)
        }

        val map = mapOf("list" to renderedPredicates)
        inInvariant = false

        return RenderResult(renderTemplate("invariants", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        fun Predicate.memberAndIdentifier(identifiers: List<Expression>): Boolean {
            if (this !is BinaryPredicate) return false
            if (operator != BinaryPredicateOperator.MEMBER) return false
            return identifiers.contains(left)
        }

        fun renderQuantification(pred: List<Predicate>): RenderResult {
            val ic = iteratorCount
            val quantificationRendered = quantification?.render()
            val resultIc = iteratorCount
            val map = mapOf(
                "body" to quantificationRendered,
                "ic" to ic,
                "resultIc" to resultIc,
                "pred" to pred.render(),
                "isConstruct" to (quantification is QuantifierPredicate)
            )

            return RenderResult(renderTemplate("quantification", map))
        }

        iteratorCount++
        val ic = iteratorCount

        val predicateList = predicate.asList()
        val iteratorIdentifier = mutableListOf<Expression>()
        val remainingPredicates = mutableListOf<Predicate>()
        val condition = mutableListOf<Predicate>()
        for (pred in predicateList) {
            if (pred is BinaryPredicate && iteratorIdentifier.contains(pred.left)) {
                condition.add(pred)
            } else if (!pred.memberAndIdentifier(identifier)) {
                condition.add(pred)
            } else {
                iteratorIdentifier.add((pred as BinaryPredicate).left)
                remainingPredicates.add(0, pred)
            }
        }

        val quantificationRendered = renderQuantification(condition)
        var currentBody = quantificationRendered
        memberAsIterator = true

        remainingPredicates.forEach {
            val id = (it as BinaryPredicate).left
            val itMap = mapOf(
                "type" to id.type.render(),
                "name" to id.render().rendered,
                "collection" to it.right.render().rendered,
                "body" to currentBody
            )
            currentBody = RenderResult(renderTemplate("iteratorConstruct", itMap))
        }

        memberAsIterator = false
        val map = mapOf(
            "body" to currentBody,
            "ic" to ic,
            "isForAll" to (type == QuantifierType.FORALL)
        )

        return RenderResult(renderTemplate(map))
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
        iteratorCount = 0

        equalsIsAssignment = true
        val renderedProperties =
            properties.filter { it is BinaryPredicate && (it.right is LambdaExpression || it.right is ComprehensionSet) }
                .render()
        equalsIsAssignment = false

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
            "properties" to renderedProperties,
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization.render(),
            "invariant" to invariant.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "transitions" to transitions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Operation.renderSelf(): RenderResult {
        currentOperation = this
        iteratorCount = 0

        val bodyUsed = (body as? Precondition)?.substitution ?: body

        val typedParameters = parameters.filterIsInstance<IdentifierExpression>()
            .map {
                renderTemplate(
                    "parameterExpression",
                    mapOf(
                        "name" to it.name,
                        "type" to it.type.render()
                    )
                )
            }

        val declarations = returnValues.filterIsInstance<IdentifierExpression>()
            .map {
                val map = mapOf(
                    "type" to it.type.render(),
                    "name" to it.name,
                    "value" to "null"
                )
                renderTemplate("declareVariable", map)
            }

        val map = mapOf(
            "name" to name,
            "parameters" to typedParameters,
            "returnValues" to returnValues.render(),
            "returnValueDeclarations" to declarations,
            "body" to bodyUsed?.render(),
            "type" to type.render()
        )

        currentOperation = null
        return RenderResult(renderTemplate(map))
    }

    override fun Transition.renderSelf(): RenderResult {
        iteratorCount = 0
        val typedParameters = parameters.filterIsInstance<IdentifierExpression>()
            .map {
                renderTemplate(
                    "parameterExpression",
                    mapOf(
                        "name" to it.name,
                        "type" to it.type.render()
                    )
                )
            }

        val bodyAsList = ArrayList<Predicate>()
        var currentPred = body
        while (currentPred is BinaryLogicPredicate) {
            if (currentPred.operator == LogicPredicateOperator.AND) {
                bodyAsList.add(0, currentPred.right)
                currentPred = currentPred.left
            } else {
                break
            }
        }
        bodyAsList.add(0, currentPred)

        val predicates = bodyAsList.map {
            val icBefore = iteratorCount + 1
            val map = mapOf(
                "predicate" to it.render(),
                "count" to icBefore,
                "isConstruct" to (it is QuantifierPredicate)
            )
            RenderResult(renderTemplate("transitionPredicate", map)).rendered
        }

        val map = mapOf(
            "name" to name,
            "parameters" to typedParameters,
            "body" to predicates
        )

        return RenderResult(renderTemplate(map))
    }


    /* ---------- TYPES AND OPERATORS ---------- */
    override fun Type?.render(): String {
        return when (this) {
            MPPCG_Boolean -> "Boolean"
            MPPCG_Nat1, MPPCG_Nat, MPPCG_Natural,
            MPPCG_Integer, MPPCG_Int -> "Integer"

            MPPCG_Real, MPPCG_Float -> "Double"
            MPPCG_String -> "String"
            MPPCG_Void -> "void"
            is TypeRelation -> "BRelation<${from.render()}, ${to.render()}>"
            is TypeSet -> "BSet<${type.render()}>"
            is TypeCouple -> "BCouple<${from.render()}, ${to.render()}>"
            is TypeOperator -> {
                when (name) {
                    "sequence" -> {
                        "BSequence<${types[0].render()}>"
                    }

                    else -> {
                        name.capitalize()
                    }
                }
            }

            else -> {
                println(this)
                throw TypeException(this!!::class.simpleName!!)
            }
        }
    }

    override fun BinaryPredicateOperator.render(): String {
        return when (this) {
            BinaryPredicateOperator.GREATER -> ">"
            BinaryPredicateOperator.GREATER_EQUAL -> ">="
            BinaryPredicateOperator.LESS -> "<"
            BinaryPredicateOperator.LESS_EQUAL -> "<="
            BinaryPredicateOperator.EQUAL -> "=="
            BinaryPredicateOperator.NOT_EQUAL -> "!="
            BinaryPredicateOperator.MEMBER -> "instanceof"
            BinaryPredicateOperator.NOT_MEMBER -> "containsNot"
            BinaryPredicateOperator.SUBSET -> "containsAll"
            BinaryPredicateOperator.STRICT_SUBSET -> "containsAllStrict??"
        }
    }

    override fun BinaryExpressionOperator.render(): String {
        return when (this) {
            BinaryExpressionOperator.ADD -> "+"
            BinaryExpressionOperator.MINUS -> "-"
            BinaryExpressionOperator.MULT -> "*"
            BinaryExpressionOperator.DIV -> "/"
            BinaryExpressionOperator.MOD -> "%"
            BinaryExpressionOperator.POW -> "**"
            BinaryExpressionOperator.PARALLEL_PRODUCT -> "mppcg_parallelProduct"
        }
    }

    override fun LogicPredicateOperator.render(): String {
        return when (this) {
            LogicPredicateOperator.AND -> "&&"
            LogicPredicateOperator.OR -> "||"
            LogicPredicateOperator.IMPLIES -> "|| !"
            LogicPredicateOperator.EQUIVALENCE -> "=="
            LogicPredicateOperator.NOT -> "!"
        }
    }

    override fun BinaryCollectionOperator.render(): String {
        return when (this) {
            BinaryCollectionOperator.INTERSECTION -> "intersection"
            BinaryCollectionOperator.SUBTRACTION -> "subtraction"
            BinaryCollectionOperator.UNION -> "union"
            BinaryCollectionOperator.CONCAT -> "concat"
            BinaryCollectionOperator.PRJ1 -> "prj1"
            BinaryCollectionOperator.PRJ2 -> "prj2"
        }
    }

    override fun BinaryFunctionOperator.render(): String {
        return when (this) {
            BinaryFunctionOperator.DOMAIN_RESTRICTION -> "domainRestriction"
            BinaryFunctionOperator.DOMAIN_SUBTRACTION -> "domainSubtraction"
            BinaryFunctionOperator.IMAGE -> "image"
            BinaryFunctionOperator.OVERWRITE -> "override"
            BinaryFunctionOperator.RANGE_RESTRICTION -> "rangeRestriction"
            BinaryFunctionOperator.RANGE_SUBTRACTION -> "rangeSubtraction"
            BinaryFunctionOperator.FORWARD_COMPOSITION -> "forwardComposition"
            BinaryFunctionOperator.ITERATE -> "iterate"
        }
    }

    override fun UnaryCollectionOperator.render(): String {
        return when (this) {
            UnaryCollectionOperator.MAX -> "max"
            UnaryCollectionOperator.MIN -> "min"
            UnaryCollectionOperator.CARD -> "card"
            UnaryCollectionOperator.POW -> "pow"
            UnaryCollectionOperator.POW1 -> "pow1"
        }
    }

    override fun CallFunctionOperator.render(): String {
        return when (this) {
            CallFunctionOperator.GET -> "get"
            CallFunctionOperator.SET -> "put"
        }
    }

    override fun UnaryFunctionOperator.render(): String {
        return when (this) {
            UnaryFunctionOperator.DOMAIN -> "domain"
            UnaryFunctionOperator.RANGE -> "range"
            UnaryFunctionOperator.REVERSE -> "inverse"
        }
    }

    override fun BinarySequenceExpressionOperator.render(): String {
        return when (this) {
            BinarySequenceExpressionOperator.RESTRICT_FRONT -> "restrict_front"
            BinarySequenceExpressionOperator.RESTRICT_TAIL -> "restrict_tail"
            BinarySequenceExpressionOperator.APPEND -> "append"
            BinarySequenceExpressionOperator.PREPEND -> "prepend"
            BinarySequenceExpressionOperator.CONCAT -> "concat"
        }
    }

    override fun UnarySequenceExpressionOperator.render(): String {
        return when (this) {
            UnarySequenceExpressionOperator.FRONT -> "front"
            UnarySequenceExpressionOperator.TAIL -> "tail"
            UnarySequenceExpressionOperator.FIRST -> "first"
            UnarySequenceExpressionOperator.LAST -> "last"
            UnarySequenceExpressionOperator.REVERSE -> "reverse"
        }
    }

    override fun UnaryExpressionOperator.render(): String {
        return when (this) {
            UnaryExpressionOperator.CONVERT_BOOLEAN -> ""
            UnaryExpressionOperator.PRED -> "pred"
            UnaryExpressionOperator.SUCC -> "succ"
            UnaryExpressionOperator.MINUS -> "-"
        }
    }

    private fun BinaryExpressionOperator.renderCustomMethod(): String {
        return when (this) {
            BinaryExpressionOperator.ADD -> "add"
            BinaryExpressionOperator.MINUS -> "minus"
            BinaryExpressionOperator.MULT -> "mult"
            BinaryExpressionOperator.DIV -> "div"
            BinaryExpressionOperator.MOD -> "mod"
            BinaryExpressionOperator.POW -> "pow"
            BinaryExpressionOperator.PARALLEL_PRODUCT -> "mppcg_parallelProduct"
        }
    }

    private fun String.capitalize(): String {
        return replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }
    }
}