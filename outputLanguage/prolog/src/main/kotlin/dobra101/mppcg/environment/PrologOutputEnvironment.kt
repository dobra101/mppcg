package dobra101.mppcg.environment

import dobra101.mppcg.IndividualInfo
import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import kotlin.math.max

class PrologOutputEnvironment : OutputLanguageEnvironment() {
    override val templateDir = "templates/prolog"
    override val fileExtension = "pl"

    internal val optimizer = PrologOptimizer(this)

    companion object {
        const val EXPRESSION_SEPARATOR = ",\n"
    }

    var exprCount = 0
    var stateCount = 0
    var operationParameters: List<IdentifierExpression> = emptyList() // HINT: only for B
    var comprehensionSetIdentifier: List<IdentifierExpression> = emptyList() // HINT: only for B
    var lambdaExpressionIdentifier: List<IdentifierExpression> = emptyList() // HINT: only for B
    var temporaryVariables: Set<IdentifierExpression> = hashSetOf() // HINT: only for B
    var concreteConstants: List<Expression> = emptyList() // HINT: only for B
    var constants: List<Expression> = emptyList() // HINT: only for B
    var usedBMethods: HashSet<CustomMethodOperator> = hashSetOf() // HINT: only for B
    val whileDefinitions: MutableList<String> = mutableListOf() // HINT: only for B
    var whileCount = 0 // HINT: only for B
    var currentOperation: String = "" // HINT: only for B

    private fun expr(name: Any): String = "Expr_$name"

    /* ---------- EXPRESSIONS ---------- */
    override fun AnonymousSetCollectionNode.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        if (elements.isEmpty()) {
            return RenderResult(
                renderTemplate(templateName, mapOf("elements" to emptyList<String>()))
            )
        }

        val expanded = ExpandedExpressionList.of(elements)
        if (expanded.before.isBlank()) {
            return RenderResult(
                renderTemplate(templateName, mapOf("elements" to expanded.expressions))
            )
        }

        val map = mapOf("elements" to expanded.expressions)
        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = rendered
        // TODO: don't remove by hand
        return RenderResult(
            expanded.before.removeSuffix(EXPRESSION_SEPARATOR),
            mapOf("resultExpr" to IndividualInfo(rendered))
        )
    }

    override fun BinaryExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val infiniteSet = ((left is InfiniteSet) || (right is InfiniteSet))
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator, infiniteSet),
            "exprCount" to exprCount,
            "math" to isMathOperator(operator),
            "infiniteSet" to infiniteSet
        )

        val rendered = renderTemplate(map)
        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        val info = mapOf("resultExpr" to IndividualInfo(expr(exprCount++))) // TODO: map to Int?
        return RenderResult("${expanded.before}$rendered", info)
    }

    override fun EnumCollectionNode.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        val map = mapOf(
            "name" to name,
            "elements" to elements.render(),
            "isParameter" to isParameter,
            "exprCount" to exprCount
        )

        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        val rendered = renderTemplate(map)
        if (isParameter) {
            val info = mapOf("resultExpr" to IndividualInfo(expr(exprCount))) // TODO: map to Int?
            exprCount++
            return RenderResult(rendered, info)
        }
        return RenderResult(rendered)
    }

    // HINT: same as SetEntry
    override fun EnumEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IdentifierExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        // TODO: not hardcoded and not always
        if (operationParameters.contains(this)
            || comprehensionSetIdentifier.contains(this)
            || lambdaExpressionIdentifier.contains(this)
        ) {
            if (optimize) optimizer.evaluated[this] = expr(name)
            return RenderResult(expr(name))
        }

        // TODO: not hardcoded and not always
        if (temporaryVariables.contains(this)) {
            if (optimize) optimizer.evaluated[this] = expr("tmp_$name")
            return RenderResult(expr("tmp_$name"))
        }

        // TODO: not hardcoded and not always
        val rendered =
            if (isConstant(this)) {
                // use constant prefix
                val cc = ConcreteIdentifierExpression(name, ValueExpression(), type)
                "${cc.name}(${expr(exprCount)})"
            } else {
                val map = mapOf(
                    "name" to name,
                    "stateCount" to stateCount,
                    "exprCount" to exprCount
                )
                renderTemplate(map)
            }
        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        val info = mapOf("resultExpr" to IndividualInfo(expr(exprCount++))) // TODO: map to Int?

        return RenderResult(rendered, info)
    }

    override fun IntervalExpression.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs
        )
        return RenderResult(renderTemplate(map), info = mapOf("before" to IndividualInfo(expanded.before)))
    }

    override fun SetCollectionNode.renderSelf(): RenderResult {
        TODO("Not yet implemented")
    }

    // HINT: same as EnumEntry
    override fun SetEntry.renderSelf(): RenderResult {
        val map = mapOf(
            "name" to name
        )

        return RenderResult(renderTemplate(map))
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
    // HINT: SAME AS LOGIC PREDICATE (except optimization)
    override fun BinaryPredicate.renderSelf(): RenderResult {
        if (optimize) optimizer.renderOptimized(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val prefixOperators = listOf(
            BinaryPredicateOperator.MEMBER,
            BinaryPredicateOperator.NOT_MEMBER,
            BinaryPredicateOperator.SUBSET
        )

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "prefixOperator" to prefixOperators.contains(operator)
        )
        val rendered = renderTemplate(map)

        // TODO: function which knows all dependencies
        if (needsCustomMethod(operator)) {
            usedBMethods.add(operator)
        }

        return RenderResult("${expanded.before}$rendered")
    }

    // HINT: SAME AS BINARY PREDICATE (except optimization)
    override fun BinaryLogicPredicate.renderSelf(): RenderResult {
        fun inline(lineBreaks: Int): Boolean {
            if (lineBreaks != 0) return false
            if (operator == LogicPredicateOperator.AND) return false
            if (left !is BinaryPredicate) return false
            if (right !is BinaryPredicate) return false
            return true
        }

        val expanded = ExpandedBinary.of(
            left,
            right,
            differentBranches = operator == LogicPredicateOperator.OR,
            environment = this@PrologOutputEnvironment
        )

        val lineBreaksTotal = expanded.lhs.count { it == '\n' } + expanded.rhs.count { it == '\n' }
        val map: MutableMap<String, Any> = mutableMapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
        )

        val rendered = when (operator) {
            LogicPredicateOperator.EQUIVALENCE -> renderTemplate("equivalence", map)
            LogicPredicateOperator.IMPLIES -> {
                map["inline"] = inline(lineBreaksTotal)
                renderTemplate("implication", map)
            }

            else -> {
                map["operator"] = operator2String(operator)
                map["addParentheses"] = operator == LogicPredicateOperator.OR
                map["inline"] = inline(lineBreaksTotal)
                renderTemplate(map)
            }
        }

        return RenderResult("${expanded.before}$rendered")
    }

    override fun UnaryLogicPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "predicate" to predicate.render(),
            "operator" to operator2String(operator)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun ValuePredicate.renderSelf(): RenderResult {
        val map = if (type is TypeBoolean) {
            mapOf("value" to (type as TypeBoolean).value?.name?.lowercase())
        } else {
            mapOf("value" to value)
        }
        return RenderResult(renderTemplate(map))
    }

    /* ---------- SUBSTITUTIONS ---------- */
    override fun AssignSubstitution.renderSelf(): RenderResult {
        val expandedRhs = ExpandedExpression.of(right)
        val map = mapOf(
            "identifier" to left.render(),
            "rhs" to expandedRhs.expression,
            "stateCount" to stateCount,
            "resultStateCount" to ++stateCount
        )
        val rendered = renderTemplate(map)

        if (optimize && !temporaryVariables.contains(left)) {
            optimizer.evaluated[left] = expandedRhs.expression
        }
        return RenderResult("${expandedRhs.before}$rendered")
    }

    override fun DeclarationSubstitution.renderSelf(): RenderResult {
        return assignment.render()
    }

    override fun ElseIfSubstitution.renderSelf(): RenderResult {
        // TODO: consider state and expr count?
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun IfSubstitution.renderSelf(): RenderResult {
        fun addStateAndExprCountFixingElseBranch(
            stateCountBefore: Int,
            exprCountBefore: Int,
            map: MutableMap<String, Any>,
            thenRendered: RenderResult
        ) {
            val elseBranch = StringBuilder()
            if (stateCountBefore < stateCount) {
                val countMap = mapOf("newCount" to stateCount, "oldCount" to stateCountBefore)
                elseBranch.append(renderTemplate("updateStateCount", countMap))
            }

            if (exprCountBefore < exprCount) {
                val countMap = mapOf("newCount" to exprCount, "oldCount" to exprCountBefore)
                if (elseBranch.isNotBlank()) {
                    elseBranch.append(EXPRESSION_SEPARATOR)
                }
                elseBranch.append(renderTemplate("updateExprCount", countMap))
            }
            map["then"] = thenRendered
            if (elseBranch.isNotBlank()) {
                map["elseSubstitution"] = elseBranch.toString()
            }
        }

        fun fixStateAndExprCountInBranches(
            thenRendered: RenderResult,
            elseRendered: RenderResult,
            stateCountAfterThen: Int,
            exprCountAfterThen: Int,
            map: MutableMap<String, Any>
        ) {
            // set the stateCount and exprCount for the main branch
            val thenStringBuilder = StringBuilder(thenRendered.rendered)
            val elseStringBuilder = StringBuilder(elseRendered.rendered)

            // stateCount
            if (stateCountAfterThen < stateCount) {
                val countMap = mapOf("newCount" to stateCount, "oldCount" to stateCountAfterThen)
                thenStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateStateCount", countMap))
            } else if (stateCountAfterThen > stateCount) {
                val countMap = mapOf("newCount" to stateCountAfterThen, "oldCount" to stateCount)
                elseStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateStateCount", countMap))
            }

            // exprCount
            if (exprCountAfterThen < exprCount) {
                val countMap = mapOf("newCount" to exprCount - 1, "oldCount" to exprCountAfterThen - 1)
                thenStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateExprCount", countMap))
            } else if (exprCountAfterThen > exprCount) {
                val countMap = mapOf("newCount" to exprCountAfterThen - 1, "oldCount" to exprCount - 1)
                elseStringBuilder
                    .append(EXPRESSION_SEPARATOR)
                    .append(renderTemplate("updateExprCount", countMap))
            }

            map["then"] = RenderResult(thenStringBuilder.toString(), thenRendered.info)
            map["elseSubstitution"] = RenderResult(elseStringBuilder.toString(), elseRendered.info)
        }

        val map = mutableMapOf(
            "condition" to condition.render(),
            "elseIf" to elseIf.render()
        )

        val stateCountBefore = stateCount
        val exprCountBefore = exprCount

        val thenRendered = then.render()

        if (elseSubstitution == null) {
            addStateAndExprCountFixingElseBranch(stateCountBefore, exprCountBefore, map, thenRendered)
        } else {
            val stateCountAfterThen = stateCount
            val exprCountAfterThen = exprCount
            stateCount = stateCountBefore
            exprCount = exprCountBefore

            val elseRendered = elseSubstitution!!.render()

            fixStateAndExprCountInBranches(
                thenRendered = thenRendered,
                elseRendered = elseRendered,
                stateCountAfterThen = stateCountAfterThen,
                exprCountAfterThen = exprCountAfterThen,
                map = map
            )

            stateCount = max(stateCountAfterThen, stateCount)
            exprCount = max(exprCountAfterThen, exprCount)
        }

        return RenderResult(renderTemplate(map))
    }

    override fun SequenceSubstitution.renderSelf(): RenderResult {
        val map = mapOf(
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun WhileSubstitution.renderSelf(): RenderResult {
        val stateCountBefore = stateCount
        val exprCountBefore = exprCount
        stateCount = 0
        exprCount = 0

        val whileDefinitionMap = mapOf(
            "name" to currentOperation,
            "count" to whileCount,
            "condition" to condition.render(),
            "body" to body.render(),
            "lastState" to stateCount
        )

        stateCount = stateCountBefore
        exprCount = exprCountBefore
        val map = mapOf(
            "name" to currentOperation,
            "count" to whileCount,
            "stateCount" to stateCount++,
            "resultStateCount" to stateCount
        )
        whileDefinitions.add(renderTemplate("whileDefinition", whileDefinitionMap))
        whileCount++

        return RenderResult(renderTemplate(map))
    }

    /* ---------- CLASS BLOCK ---------- */
    override fun ClassVariables.renderSelf(): RenderResult {
        // not needed in prolog
        return RenderResult()
    }

    /* ---------- B NODES ---------- */
    override fun Function.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(left, right)
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "type" to (type as TypeFunction).type,
            "mapType" to mapType
        )
        return RenderResult(renderTemplate(map), info = mapOf("before" to IndividualInfo(expanded.before)))
    }

    override fun BinaryCollectionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        if (needsCustomMethod(operator)) {
            usedBMethods.add(operator)
        }

        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    // TODO: reuse BinaryExpression?
    override fun BinaryFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        usedBMethods.add(operator)
        if (operator == BinaryFunctionOperator.OVERWRITE) {
            usedBMethods.add(BinaryFunctionOperator.DOMAIN_SUBTRACTION)
            usedBMethods.add(UnaryFunctionOperator.DOMAIN)
        } else if (operator == BinaryFunctionOperator.IMAGE) {
            usedBMethods.add(BinaryPredicateOperator.MEMBER)
        }

        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    override fun BinarySequenceExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val expanded = ExpandedBinary.of(left, right)

        // TODO: refactor append/prepend
        val map = mapOf(
            "lhs" to expanded.lhs,
            "rhs" to if (operator == BinarySequenceExpressionOperator.APPEND) "[${expanded.rhs}]" else expanded.rhs,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        usedBMethods.add(operator)
        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        return RenderResult("${expanded.before}$rendered", mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    override fun CallFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val before1 = ExpandedExpression.of(expression)
        val before2 = ExpandedExpressionList.of(parameters)

        val map = mapOf(
            "expression" to before1.expression,
            "parameters" to before2.expressions,
            "exprCount" to exprCount
        )

        val before = "${before1.before}${before2.before}"
        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = expr(exprCount)
        usedBMethods.add(operator)

        return RenderResult("$before$rendered", mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    override fun ComprehensionSet.renderSelf(): RenderResult {
        val comprehensionSetIdentifierBefore = comprehensionSetIdentifier
        comprehensionSetIdentifier = comprehensionSetIdentifier + identifiers.filterIsInstance<IdentifierExpression>()
        val map = mapOf(
            "identifiers" to identifiers.render(),
            "predicates" to predicates.render(),
            "exprCount" to exprCount
        )
        comprehensionSetIdentifier = comprehensionSetIdentifierBefore
        if (optimize) optimizer.evaluated[this] = expr(exprCount)

        return RenderResult(renderTemplate(map), mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    override fun ConcreteIdentifierExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()
        stateCount = 0
        exprCount = 0
        temporaryVariables = hashSetOf()

        val map = mapOf(
            "name" to name,
            "value" to value.render(),
            "inline" to (value !is ComprehensionSet && value !is LambdaExpression)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun Couple.renderSelf(): RenderResult {
        val expanded = ExpandedBinary.of(from, to)
        val map = mapOf(
            "from" to expanded.lhs,
            "to" to expanded.rhs
        )

        return RenderResult(renderTemplate(map), info = mapOf("before" to IndividualInfo(expanded.before)))
    }

    override fun InfiniteSet.renderSelf(): RenderResult {
        return RenderResult(renderTemplate(mapOf("type" to type2String(type))))
    }

    override fun LambdaExpression.renderSelf(): RenderResult {
        lambdaExpressionIdentifier = identifiers.filterIsInstance<IdentifierExpression>()
        val map = mapOf(
            "identifier" to identifiers.render(),
            "predicate" to predicate.render(),
            "expression" to expression.render(),
            "value" to expr(exprCount - 1),
            "exprCount" to exprCount
        )
        lambdaExpressionIdentifier = emptyList()

        if (optimize) optimizer.evaluated[this] = expr(exprCount)

        return RenderResult(renderTemplate(map), mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    override fun Sequence.renderSelf(): RenderResult {
        val map = mapOf(
            "elements" to elements.render()
        )
        return RenderResult(renderTemplate(map))
    }

    override fun UnarySequenceExpression.renderSelf(): RenderResult {
        val expanded = ExpandedExpression.of(sequence)
        val map = mapOf(
            "sequence" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        if (optimize) optimizer.evaluated[this] = expr(exprCount)

        usedBMethods.add(operator)

        return RenderResult(
            "${expanded.before}${renderTemplate(map)}",
            mapOf("resultExpr" to IndividualInfo(expr(exprCount++)))
        )
    }

    override fun UnaryCollectionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        val expanded = ExpandedExpression.of(collection)

        val map = mutableMapOf(
            "collection" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        if (optimize) optimizer.evaluated[this] = expr(exprCount)

        if (needsCustomMethod(operator)) {
            usedBMethods.add(operator)
            if (operator == UnaryCollectionOperator.POW) {
                usedBMethods.add(BinaryPredicateOperator.SUBSET)
            }
        }

        return RenderResult(
            "${expanded.before}${renderTemplate(map)}",
            mapOf("resultExpr" to IndividualInfo(expr(exprCount++)))
        )
    }

    override fun UnaryExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }

        val map = mapOf(
            "value" to value.render(),
            "operator" to operator2String(operator),
            "convertBoolean" to (operator == UnaryExpressionOperator.CONVERT_BOOLEAN),
            "exprCount" to exprCount
        )
        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = expr(exprCount)

        return RenderResult(rendered, mapOf("resultExpr" to IndividualInfo(expr(exprCount++))))
    }

    override fun UnaryFunctionExpression.renderSelf(): RenderResult {
        if (optimize) optimizer.loadIfEvaluated(this)?.let { return it }
        usedBMethods.add(operator)

        val expanded = ExpandedExpression.of(expression)

        val map = mapOf(
            "expression" to expanded.expression,
            "operator" to operator2String(operator),
            "exprCount" to exprCount
        )

        val rendered = renderTemplate(map)

        if (optimize) optimizer.evaluated[this] = expr(exprCount)

        return RenderResult(
            "${expanded.before}$rendered",
            mapOf("resultExpr" to IndividualInfo(expr(exprCount++)))
        )
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Invariant.renderSelf(): RenderResult {
        val checkInvs = List(predicates.size) { idx ->
            if (optimize) optimizer.evaluated = hashMapOf()
            exprCount = 0
            stateCount = 0
            val map = mapOf(
                "body" to predicates[idx].render(),
                "idx" to idx
            )
            renderTemplate(map)
        }

        val renderedCheckInvs = renderTemplate("invariants", mapOf("list" to checkInvs))

        val map = mapOf(
            "invariants" to renderedCheckInvs,
            "indices" to predicates.indices.map { it }
        )


        return RenderResult(renderTemplate("props", map))
    }

    override fun QuantifierPredicate.renderSelf(): RenderResult {
        val map = mapOf(
            "identifier" to identifier.render(),
            "predicate" to predicate.render(),
            "quantification" to quantification?.render(),
            "universalQuantifier" to (type == QuantifierType.FORALL)
        )
        return RenderResult(renderTemplate(map))
    }

    override fun Initialization.renderSelf(): RenderResult {
        val map = mapOf(
            "body" to substitutions.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun ParallelSubstitution.renderSelf(): RenderResult {
        temporaryVariables = needTempVar

        // remove temporary variables from previous evaluated to use temp vars
        if (optimize) temporaryVariables.forEach { optimizer.evaluated.remove(it) }

        val neededTempVars = substitutions
            .filterIsInstance<AssignSubstitution>()
            .filter { it.right is IdentifierExpression && temporaryVariables.contains(it.right) }
            .map { it.right as IdentifierExpression }

        // TODO: let optimizer do the work
        // load temporary variables if they are needed
        // optimized: they are not needed if e.g. 'a = a + 1 || b = a + 1' and a + 1 has been evaluated before
        val tempVars = needTempVar.mapNotNull {
            if (optimize && !neededTempVars.contains(it)) {
                null
            } else {
                val map = mapOf(
                    "name" to it.name,
                    "stateCount" to stateCount,
                    "exprCount" to "tmp_${it.name}"
                )
                renderTemplate(it.templateName, map)
            }
        }

        val map = mapOf(
            "tempVars" to tempVars,
            "substitutions" to substitutions.render()
        )

        return RenderResult(renderTemplate(map))
    }

    // HINT: SAME FOR JAVA AND PROLOG
    override fun Precondition.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()

        val map = mapOf(
            "predicate" to predicate.render().rendered,
            "substitution" to (substitution?.render()?.rendered ?: "")
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Select.renderSelf(): RenderResult {
        // TODO: add when, else
        val map = mapOf(
            "condition" to condition.render(),
            "then" to then?.render()
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Machine.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()
        this@PrologOutputEnvironment.concreteConstants = concreteConstants
        this@PrologOutputEnvironment.constants = constants
        stateCount = 0
        exprCount = 0
        operationParameters = parameters.map { it as IdentifierExpression }
        temporaryVariables = hashSetOf()

        val map = mapOf(
            "name" to name,
            "parameters" to parameters.render(),
            "constraints" to constraints?.render(),
            "sets" to sets.render(),
            "constants" to constants.render(),
            "concrete_constants" to concreteConstants.render(),
//            "properties" to properties?.render(), // TODO: use properties
            "definitions" to definitions?.render(),
            "variables" to variables.render(),
            "concrete_variables" to concreteVariables.render(),
            "initialization" to initialization?.render(),
            "invariant" to invariant.render(),
            "assertions" to assertions.render(),
            "operations" to operations.render(),
            "methods" to usedBMethods.render(),
            "whileDefinitions" to whileDefinitions
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Operation.renderSelf(): RenderResult {
        if (optimize) optimizer.evaluated = hashMapOf()
        stateCount = 0
        exprCount = 0
        operationParameters = parameters.map { it as IdentifierExpression }
        temporaryVariables = hashSetOf()
        currentOperation = name
        whileCount = 0

        val map = mapOf(
            "name" to name,
            "parameters" to operationParameters.map { it.name.lowercase() },
            "returnValues" to returnValues.map { (it as? IdentifierExpression)?.name }, // TODO: add returnValues?
            "body" to body?.render(),
            "resultStateCount" to stateCount
        )

        return RenderResult(renderTemplate(map))
    }

    override fun Transition.renderSelf(): RenderResult {
        // not needed because of XTL
        return RenderResult("")
    }

    // TODO: are all operators math-operators?
    private fun isMathOperator(operator: BinaryExpressionOperator): Boolean {
        return when (operator) {
            BinaryExpressionOperator.ADD -> true
            BinaryExpressionOperator.MINUS -> true
            BinaryExpressionOperator.MULT -> true
            BinaryExpressionOperator.DIV -> true
            BinaryExpressionOperator.MOD -> true
        }
    }

    private fun operator2String(operator: BinaryExpressionOperator, infiniteSet: Boolean): String {
        if (infiniteSet) {
            return when (operator) {
                BinaryExpressionOperator.ADD -> "mppcg_add"
                BinaryExpressionOperator.MINUS -> "mppcg_minus"
                BinaryExpressionOperator.MULT -> "mppcg_mult"
                BinaryExpressionOperator.DIV -> "mppcg_div"
                BinaryExpressionOperator.MOD -> "mppcg_mod"
            }
        }
        return operator2String(operator)
    }

    override fun operator2String(operator: BinaryExpressionOperator): String {
        return when (operator) {
            BinaryExpressionOperator.MOD -> "mod"
            BinaryExpressionOperator.DIV -> "//"
            else -> super.operator2String(operator)
        }
    }

    override fun operator2String(operator: LogicPredicateOperator): String {
        return when (operator) {
            LogicPredicateOperator.AND -> ", "
            LogicPredicateOperator.OR -> "; "
            LogicPredicateOperator.IMPLIES -> " -> "
            LogicPredicateOperator.EQUIVALENCE -> " = "
            LogicPredicateOperator.NOT -> "\\+ "
        }
    }

    private fun needsCustomMethod(operator: CustomMethodOperator): Boolean {
        if (operator is BMethod) return true
        return when (operator) {
            BinaryPredicateOperator.MEMBER,
            BinaryPredicateOperator.SUBSET -> true

            is BinaryCollectionOperator -> true
            is UnaryCollectionOperator -> true

            else -> false
        }
    }

    internal fun isConstant(identifierExpression: IdentifierExpression): Boolean {
        if (constants.contains(identifierExpression)) return true
        if (concreteConstants.contains(identifierExpression)) return true

        val cc = ConcreteIdentifierExpression(identifierExpression.name, ValueExpression(), identifierExpression.type)
        return concreteConstants.find { (it as? ConcreteIdentifierExpression)?.name == cc.name } != null
    }

    // HINT: input language specific
    private fun HashSet<CustomMethodOperator>.render(): List<String> {
        return map {
            when (it) {
                is UnaryCollectionOperator -> it.render()
                is UnaryFunctionOperator -> it.render()
                is BinaryCollectionOperator -> it.render()
                is BinaryFunctionOperator -> it.render()
                is BinaryPredicateOperator -> it.render()
                is BinarySequenceExpressionOperator -> it.render()
                is CallFunctionOperator -> it.render()
                is UnarySequenceExpressionOperator -> it.render()
                else -> throw EnvironmentException("Rendering of custom method for operator '$it' (${it::class.simpleName}) is not implemented.")
            }
        }
    }

    private fun UnaryCollectionOperator.render(): String {
        return when (this) {
            UnaryCollectionOperator.CARD -> renderTemplate(
                "cardinality",
                mapOf(
                    "name" to operator2String(this)
                )
            )

            UnaryCollectionOperator.POW -> renderTemplate(
                "powerSet",
                mapOf(
                    "name" to operator2String(this),
                    "subsetName" to operator2String(BinaryPredicateOperator.SUBSET)
                )
            )

            UnaryCollectionOperator.POW1 -> renderTemplate(
                "powerSet1",
                mapOf(
                    "name" to operator2String(this),
                    "subsetName" to operator2String(BinaryPredicateOperator.SUBSET)
                )
            )

            UnaryCollectionOperator.MAX -> renderTemplate(
                "maxSet",
                mapOf(
                    "name" to operator2String(this)
                )
            )

            UnaryCollectionOperator.MIN -> renderTemplate(
                "minSet",
                mapOf(
                    "name" to operator2String(this)
                )
            )

            else -> throw EnvironmentException("Rendering of custom method for operator '${name}' (${this::class.simpleName}) is not implemented.")
        }
    }

    // HINT: input language specific
    private fun UnaryFunctionOperator.render(): String {
        return when (this) {
            UnaryFunctionOperator.DOMAIN -> renderTemplate("domain", mapOf("name" to operator2String(this)))
            UnaryFunctionOperator.RANGE -> renderTemplate("range", mapOf("name" to operator2String(this)))
            UnaryFunctionOperator.REVERSE -> renderTemplate("reverse", mapOf("name" to operator2String(this)))
            else -> throw EnvironmentException("Rendering of custom method for operator '${name}' (${this::class.simpleName}) is not implemented.")
        }
    }

    // HINT: input language specific
    private fun BinaryFunctionOperator.render(): String {
        return when (this) {
            BinaryFunctionOperator.DOMAIN_RESTRICTION -> renderTemplate(
                "domainRestriction",
                mapOf("name" to operator2String(this))
            )

            BinaryFunctionOperator.DOMAIN_SUBTRACTION -> renderTemplate(
                "domainSubtraction",
                mapOf("name" to operator2String(this))
            )

            BinaryFunctionOperator.IMAGE -> renderTemplate(
                "image",
                mapOf(
                    "name" to operator2String(this),
                    "memberName" to operator2String(BinaryPredicateOperator.MEMBER)
                )
            )

            BinaryFunctionOperator.OVERWRITE -> renderTemplate(
                "overwrite", mapOf(
                    "name" to operator2String(this),
                    "domainName" to operator2String(UnaryFunctionOperator.DOMAIN),
                    "domainSubtractionName" to operator2String(BinaryFunctionOperator.DOMAIN_SUBTRACTION)
                )
            )

            BinaryFunctionOperator.RANGE_RESTRICTION -> renderTemplate(
                "rangeRestriction",
                mapOf("name" to operator2String(this))
            )

            BinaryFunctionOperator.RANGE_SUBTRACTION -> renderTemplate(
                "rangeSubtraction",
                mapOf("name" to operator2String(this))
            )

            BinaryFunctionOperator.FORWARD_COMPOSITION -> renderTemplate(
                "forwardComposition",
                mapOf("name" to operator2String(this), "memberName" to operator2String(BinaryPredicateOperator.MEMBER))
            )

            else -> throw EnvironmentException("Rendering of custom method for operator '${name}' (${this::class.simpleName}) is not implemented.")
        }
    }

    // HINT: input language specific
    private fun BinaryPredicateOperator.render(): String {
        return when (this) {
            BinaryPredicateOperator.MEMBER -> renderTemplate("member", mapOf("name" to operator2String(this)))
            BinaryPredicateOperator.SUBSET -> renderTemplate("subset", mapOf("name" to operator2String(this)))

            else -> throw EnvironmentException("Rendering of custom method for operator '${name}' (${this::class.simpleName}) is not implemented.")
        }
    }

    private fun BinaryCollectionOperator.render(): String {
        return when (this) {
            BinaryCollectionOperator.SUBTRACTION -> renderTemplate(
                "setSubtraction",
                mapOf("name" to operator2String(this))
            )

            BinaryCollectionOperator.INTERSECTION -> renderTemplate(
                "setIntersection",
                mapOf("name" to operator2String(this))
            )

            BinaryCollectionOperator.UNION -> renderTemplate(
                "setUnion",
                mapOf("name" to operator2String(this))
            )

            BinaryCollectionOperator.CONCAT -> renderTemplate(
                "setConcat",
                mapOf("name" to operator2String(this))
            )

            BinaryCollectionOperator.PRJ1 -> renderTemplate(
                "setPrj1",
                mapOf("name" to operator2String(this))
            )

            BinaryCollectionOperator.PRJ2 -> renderTemplate(
                "setPrj2",
                mapOf("name" to operator2String(this))
            )
        }
    }

    private fun BinarySequenceExpressionOperator.render(): String {
        return when (this) {
            BinarySequenceExpressionOperator.RESTRICT_FRONT -> renderTemplate(
                "sequenceRestrictFront",
                mapOf("name" to operator2String(this))
            )

            BinarySequenceExpressionOperator.RESTRICT_TAIL -> renderTemplate(
                "sequenceRestrictTail",
                mapOf("name" to operator2String(this))
            )

            else -> ""
        }
    }

    private fun CallFunctionOperator.render(): String {
        return renderTemplate(
            "callFunctionPredicate",
            mapOf("memberName" to operator2String(BinaryPredicateOperator.MEMBER))
        )
    }

    private fun UnarySequenceExpressionOperator.render(): String {
        return renderTemplate("sequenceFront", mapOf("name" to operator2String(this)))
    }

    override fun type2String(type: Type?): String {
        return when (type) {
            is TypeNatural -> "'NAT'"
            is TypeInteger -> "'INT'"
            is TypeSet -> type2String(type.type)
            is TypeBoolean -> "'BOOL'"
            else -> TODO("Infinite Set not implemented (${type!!::class.simpleName})")
        }
    }
}

// TODO: rename?
private data class ExpandedExpressionList(val before: String = "", val expressions: List<String> = emptyList()) {
    companion object {
        fun of(expressions: List<Expression>): ExpandedExpressionList {
            var before = ""
            val expression = expressions.map {
                val result = it.render()

                if (it is ValueExpression || it is SetEntry) {
                    result.rendered
                } else {
                    if (result.rendered.isNotBlank() && result.containsKey("resultExpr")) {
                        before += "${result.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                    }
                    if (result.containsKey("before")) {
                        before += result["before"].info
                    }
                    if (result.info.containsKey("resultExpr")) result["resultExpr"].info
                    else result.rendered
                }
            }
            return ExpandedExpressionList(before, expression)
        }
    }
}

// TODO: use expandExpression for each side
private data class ExpandedBinary(val before: String = "", val lhs: String = "", val rhs: String = "") {
    companion object {
        fun of(
            left: MPPCGNode,
            right: MPPCGNode,
            differentBranches: Boolean = false,
            environment: PrologOutputEnvironment? = null
        ): ExpandedBinary {
            // true copy of map
            val evaluatedBefore = environment?.optimizer?.evaluated?.toMutableMap() as? HashMap<MPPCGNode, String>

            val lhsRendered = left.render()
            if (differentBranches && environment != null) environment.optimizer.evaluated = evaluatedBefore!!
            val rhsRendered = right.render()
            if (differentBranches && environment != null) environment.optimizer.evaluated = evaluatedBefore!!

            var before = ""
            val lhs: String
            val rhs: String
            if (lhsRendered.containsKey("resultExpr")) { // TODO: replace result expr by variable to avoid misspelling
                lhs = lhsRendered["resultExpr"].info
                if (lhsRendered.rendered.isNotBlank()) {
                    before += "${lhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                }
            } else {
                lhs = lhsRendered.rendered
            }
            if (lhsRendered.containsKey("before")) {
                before += lhsRendered["before"].info
            }

            if (rhsRendered.containsKey("resultExpr")) {
                rhs = rhsRendered["resultExpr"].info
                if (rhsRendered.rendered.isNotBlank()) {
                    before += "${rhsRendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                }
            } else {
                rhs = rhsRendered.rendered
            }
            if (rhsRendered.containsKey("before")) {
                before += rhsRendered["before"].info
            }
            return ExpandedBinary(before, lhs, rhs)
        }
    }
}

private data class ExpandedExpression(val before: String = "", val expression: String = "") {
    companion object {
        fun of(expression: Expression): ExpandedExpression {
            val expanded = expression.render()

            val before = if (expanded.rendered.isNotBlank() && expanded.containsKey("resultExpr")) {
                "${expanded.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
            } else {
                ""
            }

            val expr = if (expanded.containsKey("resultExpr")) {
                expanded["resultExpr"].info
            } else {
                expanded.rendered
            }

            return ExpandedExpression(before, expr)
        }
    }
}