package dobra101.mppcg.environment

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.collection.BinaryCollectionOperator
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.collection.UnaryCollectionOperator
import dobra101.mppcg.node.expression.BinaryExpressionOperator
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.predicate.BinaryPredicateOperator
import dobra101.mppcg.node.predicate.LogicPredicateOperator
import dobra101.mppcg.node.predicate.Predicate
import dobra101.mppcg.node.substitution.Substitution

abstract class EnvironmentUtils {
    open fun operator2String(operator: BinaryPredicateOperator): String {
        return when (operator) {
            BinaryPredicateOperator.GREATER -> ">"
            BinaryPredicateOperator.GREATER_EQUAL -> ">="
            BinaryPredicateOperator.LESS -> "<"
            BinaryPredicateOperator.LESS_EQUAL -> "=<"
            BinaryPredicateOperator.EQUAL -> "="
            BinaryPredicateOperator.NOT_EQUAL -> "\\="
            BinaryPredicateOperator.MEMBER -> "mppcg_member"
            BinaryPredicateOperator.NOT_MEMBER -> "\\+ ${operator2String(BinaryPredicateOperator.MEMBER)}"
            BinaryPredicateOperator.SUBSET -> "mppcg_subset"
        }
    }

    open fun operator2String(operator: BinaryExpressionOperator): String {
        return when (operator) {
            BinaryExpressionOperator.ADD -> "+"
            BinaryExpressionOperator.MINUS -> "-"
            BinaryExpressionOperator.MULT -> "*"
            BinaryExpressionOperator.DIV -> "/"
            BinaryExpressionOperator.MOD -> "%"
        }
    }

    open fun operator2String(operator: LogicPredicateOperator): String {
        return when (operator) {
            LogicPredicateOperator.AND -> "&&"
            LogicPredicateOperator.OR -> "||"
            LogicPredicateOperator.IMPLIES -> "=>"
            LogicPredicateOperator.EQUIVALENCE -> "<=>"
            LogicPredicateOperator.NOT -> "!"
        }
    }

    open fun operator2String(operator: BinaryCollectionOperator): String {
        return when (operator) {
            BinaryCollectionOperator.INTERSECTION -> "intersection"
            BinaryCollectionOperator.SUBTRACTION -> "subtraction"
            BinaryCollectionOperator.UNION -> "union"
        }
    }

    // TODO: names as constants in some file to avoid generation of different named functions
    open fun operator2String(operator: BinaryFunctionOperator): String {
        return when (operator) {
            BinaryFunctionOperator.DOMAIN_RESTRICTION -> "domainRestriction"
            BinaryFunctionOperator.DOMAIN_SUBTRACTION -> "domainSubtraction"
            BinaryFunctionOperator.IMAGE -> "image"
            BinaryFunctionOperator.OVERWRITE -> "overwrite"
            BinaryFunctionOperator.RANGE_RESTRICTION -> "rangeRestriction"
            BinaryFunctionOperator.RANGE_SUBTRACTION -> "rangeSubtraction"
            BinaryFunctionOperator.FORWARD_COMPOSITION -> "forwardComposition"
        }
    }

    open fun operator2String(operator: UnaryCollectionOperator): String {
        return when (operator) {
            UnaryCollectionOperator.MAX -> "max"
            UnaryCollectionOperator.MIN -> "min"
            UnaryCollectionOperator.CARD -> "card"
            UnaryCollectionOperator.POW -> "pow"
            UnaryCollectionOperator.POW1 -> "pow1"
        }
    }

    open fun operator2String(operator: UnaryFunctionOperator): String {
        return when (operator) {
            UnaryFunctionOperator.DOMAIN -> "domain"
            UnaryFunctionOperator.RANGE -> "range"
            UnaryFunctionOperator.REVERSE -> "reverse"
        }
    }

    open fun operator2String(operator: BinarySequenceExpressionOperator): String {
        return when (operator) {
            BinarySequenceExpressionOperator.RESTRICT_FRONT -> "restrict_front"
            BinarySequenceExpressionOperator.RESTRICT_TAIL -> "restrict_tail"
            BinarySequenceExpressionOperator.APPEND -> "append"
            BinarySequenceExpressionOperator.PREPEND -> "prepend"
        }
    }

    open fun operator2String(operator: UnarySequenceExpressionOperator): String {
        return when (operator) {
            UnarySequenceExpressionOperator.FRONT -> "front"
            UnarySequenceExpressionOperator.TAIL -> "tail"
            UnarySequenceExpressionOperator.FIRST -> "first"
            UnarySequenceExpressionOperator.LAST -> "last"
            UnarySequenceExpressionOperator.REVERSE -> "reverse"
        }
    }

    abstract fun type2String(type: Type?): String

    @JvmName("renderExpressionList")
    fun List<Expression>.render(): List<String> {
        return this.map { it.render().rendered }
    }

    @JvmName("renderOperationList")
    fun List<Operation>.render(): List<String> {
        return this.map { it.render().rendered }
    }

    @JvmName("renderTransitionList")
    fun List<Transition>.render(): List<String> {
        return this.map { it.render().rendered }
    }

    @JvmName("renderSubstitutionList")
    fun List<Substitution>.render(): List<String> {
        return this.map { it.render().rendered }
    }

    @JvmName("renderPredicateList")
    fun List<Predicate>.render(): List<String> {
        return this.map { it.render().rendered }
    }

    @JvmName("renderMachineCollectionNodeList")
    fun List<CollectionNode>.render(): List<String> {
        return this.map { it.render().rendered }
    }

    @JvmName("renderIdentifierList")
    fun List<IdentifierExpression>.render(): List<String> {
        return this.map { it.render().rendered }
    }
}