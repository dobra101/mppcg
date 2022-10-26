package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.b.FunctionMapType
import dobra101.mppcg.node.b.FunctionType
import dobra101.mppcg.node.b.InfiniteSet
import dobra101.mppcg.node.expression.*

class ExpressionVisitor : AbstractVisitor() {

    override var result: Expression? = null

    override fun caseTIdentifierLiteral(node: TIdentifierLiteral) {
        result = IdentifierExpression(node.text, type = TypeInteger()) // TODO: fix type
    }

    override fun caseTIntegerLiteral(node: TIntegerLiteral) {
        result = ValueExpression(node.text, type = TypeInteger())
    }

    override fun caseTStringLiteral(node: TStringLiteral) {
        result = ValueExpression(node.text, type = TypeString())
    }

    override fun caseTRealLiteral(node: TRealLiteral) {
        result = ValueExpression(node.text, type = TypeReal())
    }

    override fun caseAAddExpression(node: AAddExpression) {
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.ADD)
    }

    override fun caseAMinusExpression(node: AMinusExpression) {
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.MINUS)
    }

    // TODO: fix for sets
    override fun caseAMinusOrSetSubtractExpression(node: AMinusOrSetSubtractExpression) {
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.MINUS)
    }

    override fun caseATotalInjectionExpression(node: ATotalInjectionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.INJECTION
        )
    }

    override fun caseAPartialInjectionExpression(node: APartialInjectionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.INJECTION
        )
    }

    override fun caseATotalBijectionExpression(node: ATotalBijectionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.BIJECTION
        )
    }

    override fun caseAPartialBijectionExpression(node: APartialBijectionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.BIJECTION
        )
    }

    override fun caseATotalSurjectionExpression(node: ATotalSurjectionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.SURJECTION
        )
    }

    override fun caseAPartialSurjectionExpression(node: APartialSurjectionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.SURJECTION
        )
    }

    override fun caseATotalFunctionExpression(node: ATotalFunctionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.FUNCTION
        )
    }

    override fun caseAPartialFunctionExpression(node: APartialFunctionExpression) {
        result = Function(
            left = node.left.convert()!!,
            right = node.right.convert()!!,
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.FUNCTION
        )
    }

    override fun caseAIntervalExpression(node: AIntervalExpression) {
        result = IntervalExpression(node.leftBorder.convert()!!, node.rightBorder.convert()!!)
    }

    override fun caseAIntegerSetExpression(node: AIntegerSetExpression) {
        result = InfiniteSet(TypeSet(SetType.INTEGER))
    }

    override fun caseANaturalSetExpression(node: ANaturalSetExpression) {
        result = InfiniteSet(TypeSet(SetType.NATURAL))
    }
}