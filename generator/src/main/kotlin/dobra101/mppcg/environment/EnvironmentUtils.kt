package dobra101.mppcg.environment

import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.collection.BinaryCollectionOperator
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.collection.UnaryCollectionOperator
import dobra101.mppcg.node.expression.BinaryExpressionOperator
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.expression.UnaryExpressionOperator
import dobra101.mppcg.node.Type
import dobra101.mppcg.node.predicate.BinaryPredicateOperator
import dobra101.mppcg.node.predicate.LogicPredicateOperator
import dobra101.mppcg.node.predicate.Predicate
import dobra101.mppcg.node.substitution.Substitution

/**
 * Utility class to provide mode extension functions.
 *
 * For example, abstract extension functions for rendering operator names are provided by this class.
 */
abstract class EnvironmentUtils {
    abstract fun BinaryPredicateOperator.render(): String
    abstract fun BinaryExpressionOperator.render(): String
    abstract fun LogicPredicateOperator.render(): String
    abstract fun BinaryCollectionOperator.render(): String
    abstract fun BinaryFunctionOperator.render(): String
    abstract fun UnaryCollectionOperator.render(): String
    abstract fun CallFunctionOperator.render(): String
    abstract fun UnaryFunctionOperator.render(): String
    abstract fun BinarySequenceExpressionOperator.render(): String
    abstract fun UnarySequenceExpressionOperator.render(): String
    abstract fun UnaryExpressionOperator.render(): String

    abstract fun Type?.render(): String

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