package dobra101.mppcg.environment

import dobra101.mppcg.node.Type
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.b.Transition
import dobra101.mppcg.node.collection.CollectionNode
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
            BinaryPredicateOperator.MEMBER -> "member"
            BinaryPredicateOperator.NOT_MEMBER -> "notmember"
            BinaryPredicateOperator.SUBSET -> "subset"
        }
    }

    open fun operator2String(operator: BinaryExpressionOperator): String {
        return when (operator) {
            BinaryExpressionOperator.ADD -> "+"
            BinaryExpressionOperator.MINUS -> "-"
            BinaryExpressionOperator.MULT -> "*"
            BinaryExpressionOperator.DIV -> "/"
        }
    }

    open fun operator2String(operator: LogicPredicateOperator): String {
        return when (operator) {
            LogicPredicateOperator.AND -> "&&"
            LogicPredicateOperator.OR -> "||"
            LogicPredicateOperator.IMPLIES -> "=>"
        }
    }

    abstract fun type2String(type: Type): String

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