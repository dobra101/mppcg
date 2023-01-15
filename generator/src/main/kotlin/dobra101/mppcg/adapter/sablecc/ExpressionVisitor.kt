package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*

class ExpressionVisitor : AbstractVisitor() {

    override var result: Expression? = null

    override fun caseTIdentifierLiteral(node: TIdentifierLiteral) {
        result = machineVisitor.sets.findByName(node.text) ?: machineVisitor.sets.findEntryByName(node.text)
                ?: IdentifierExpression(name = node.text, type = TypeInteger()) // TODO: fix type
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

    override fun caseASetExtensionExpression(node: ASetExtensionExpression) {
        val expressions = node.expressions.convert()
        result = AnonymousSetCollectionNode(expressions)
    }

    override fun caseAEmptySetExpression(node: AEmptySetExpression) {
        result = AnonymousSetCollectionNode()
    }

    override fun caseAReverseExpression(node: AReverseExpression) {
        result = ReverseFunctionExpression(node.expression.convert()!!)
    }

    override fun caseAFileExpression(node: AFileExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADescriptionExpression(node: ADescriptionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIdentifierExpression(node: AIdentifierExpression) {
        // not needed -> see caseTIdentifierLiteral
        super.caseAIdentifierExpression(node)
    }

    override fun caseAPrimedIdentifierExpression(node: APrimedIdentifierExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAStringExpression(node: AStringExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMultilineStringExpression(node: AMultilineStringExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABooleanTrueExpression(node: ABooleanTrueExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABooleanFalseExpression(node: ABooleanFalseExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIntegerExpression(node: AIntegerExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARealExpression(node: ARealExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAHexIntegerExpression(node: AHexIntegerExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMaxIntExpression(node: AMaxIntExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMinIntExpression(node: AMinIntExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARealSetExpression(node: ARealSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFloatSetExpression(node: AFloatSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseANatural1SetExpression(node: ANatural1SetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseANatSetExpression(node: ANatSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseANat1SetExpression(node: ANat1SetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIntSetExpression(node: AIntSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABoolSetExpression(node: ABoolSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAStringSetExpression(node: AStringSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConvertBoolExpression(node: AConvertBoolExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAUnaryMinusExpression(node: AUnaryMinusExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMultiplicationExpression(node: AMultiplicationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACartesianProductExpression(node: ACartesianProductExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMultOrCartExpression(node: AMultOrCartExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADivExpression(node: ADivExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFlooredDivExpression(node: AFlooredDivExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIfElsifExprExpression(node: AIfElsifExprExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIfThenElseExpression(node: AIfThenElseExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALetExpressionExpression(node: ALetExpressionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAModuloExpression(node: AModuloExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPowerOfExpression(node: APowerOfExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASuccessorExpression(node: ASuccessorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPredecessorExpression(node: APredecessorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMaxExpression(node: AMaxExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMinExpression(node: AMinExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACardExpression(node: ACardExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConvertIntFloorExpression(node: AConvertIntFloorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConvertIntCeilingExpression(node: AConvertIntCeilingExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConvertRealExpression(node: AConvertRealExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAGeneralSumExpression(node: AGeneralSumExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAGeneralProductExpression(node: AGeneralProductExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACoupleExpression(node: ACoupleExpression) {
        result = Couple(node.list.convert())
    }

    override fun caseAComprehensionSetExpression(node: AComprehensionSetExpression) {
        result = ComprehensionSet(node.identifiers.convert(), node.predicates.convert()!!)
    }

    override fun caseASymbolicComprehensionSetExpression(node: ASymbolicComprehensionSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAProverComprehensionSetExpression(node: AProverComprehensionSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBComprehensionSetExpression(node: AEventBComprehensionSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPowSubsetExpression(node: APowSubsetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPow1SubsetExpression(node: APow1SubsetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFinSubsetExpression(node: AFinSubsetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFin1SubsetExpression(node: AFin1SubsetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFiniteExpression(node: AFiniteExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAUnionExpression(node: AUnionExpression) {
        result = BinaryCollectionExpression(node.left.convert()!!, node.right.convert()!!, BinaryCollectionOperator.UNION)
    }

    override fun caseAIntersectionExpression(node: AIntersectionExpression) {
        result = BinaryCollectionExpression(node.left.convert()!!, node.right.convert()!!, BinaryCollectionOperator.INTERSECTION)
    }

    override fun caseASetSubtractionExpression(node: ASetSubtractionExpression) {
        result = BinaryCollectionExpression(node.left.convert()!!, node.right.convert()!!, BinaryCollectionOperator.SUBTRACTION)
    }

    override fun caseAGeneralUnionExpression(node: AGeneralUnionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAGeneralIntersectionExpression(node: AGeneralIntersectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAQuantifiedUnionExpression(node: AQuantifiedUnionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASymbolicQuantifiedUnionExpression(node: ASymbolicQuantifiedUnionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAQuantifiedIntersectionExpression(node: AQuantifiedIntersectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARelationsExpression(node: ARelationsExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIdentityExpression(node: AIdentityExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBIdentityExpression(node: AEventBIdentityExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFirstProjectionExpression(node: AFirstProjectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBFirstProjectionExpression(node: AEventBFirstProjectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBFirstProjectionV2Expression(node: AEventBFirstProjectionV2Expression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASecondProjectionExpression(node: ASecondProjectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBSecondProjectionExpression(node: AEventBSecondProjectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBSecondProjectionV2Expression(node: AEventBSecondProjectionV2Expression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACompositionExpression(node: ACompositionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASymbolicCompositionExpression(node: ASymbolicCompositionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARingExpression(node: ARingExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADirectProductExpression(node: ADirectProductExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAParallelProductExpression(node: AParallelProductExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIterationExpression(node: AIterationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAReflexiveClosureExpression(node: AReflexiveClosureExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAClosureExpression(node: AClosureExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADomainExpression(node: ADomainExpression) {
        result = DomainFunctionExpression(node.expression.convert()!!)
    }

    override fun caseARangeExpression(node: ARangeExpression) {
        result = RangeFunctionExpression(node.expression.convert()!!)
    }

    override fun caseAImageExpression(node: AImageExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.IMAGE)
    }

    override fun caseADomainRestrictionExpression(node: ADomainRestrictionExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.DOMAIN_RESTRICTION)
    }

    override fun caseADomainSubtractionExpression(node: ADomainSubtractionExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.DOMAIN_SUBTRACTION)
    }

    override fun caseARangeRestrictionExpression(node: ARangeRestrictionExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.RANGE_RESTRICTION)
    }

    override fun caseARangeSubtractionExpression(node: ARangeSubtractionExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.RANGE_SUBTRACTION)
    }

    override fun caseAOverwriteExpression(node: AOverwriteExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATotalRelationExpression(node: ATotalRelationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASurjectionRelationExpression(node: ASurjectionRelationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATotalSurjectionRelationExpression(node: ATotalSurjectionRelationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALambdaExpression(node: ALambdaExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASymbolicLambdaExpression(node: ASymbolicLambdaExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATransFunctionExpression(node: ATransFunctionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATransRelationExpression(node: ATransRelationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASeqExpression(node: ASeqExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASeq1Expression(node: ASeq1Expression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIseqExpression(node: AIseqExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIseq1Expression(node: AIseq1Expression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPermExpression(node: APermExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEmptySequenceExpression(node: AEmptySequenceExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASequenceExtensionExpression(node: ASequenceExtensionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASizeExpression(node: ASizeExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFirstExpression(node: AFirstExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALastExpression(node: ALastExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFrontExpression(node: AFrontExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATailExpression(node: ATailExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARevExpression(node: ARevExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConcatExpression(node: AConcatExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInsertFrontExpression(node: AInsertFrontExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInsertTailExpression(node: AInsertTailExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARestrictFrontExpression(node: ARestrictFrontExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARestrictTailExpression(node: ARestrictTailExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAGeneralConcatExpression(node: AGeneralConcatExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADefinitionExpression(node: ADefinitionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFunctionExpression(node: AFunctionExpression) {
        result = CallFunctionExpression(node.identifier.convert()!!, node.parameters.convert())
    }

    override fun caseATreeExpression(node: ATreeExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABtreeExpression(node: ABtreeExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConstExpression(node: AConstExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATopExpression(node: ATopExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASonsExpression(node: ASonsExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPrefixExpression(node: APrefixExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPostfixExpression(node: APostfixExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASizetExpression(node: ASizetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMirrorExpression(node: AMirrorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARankExpression(node: ARankExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFatherExpression(node: AFatherExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASonExpression(node: ASonExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASubtreeExpression(node: ASubtreeExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAArityExpression(node: AArityExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABinExpression(node: ABinExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALeftExpression(node: ALeftExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARightExpression(node: ARightExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInfixExpression(node: AInfixExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAStructExpression(node: AStructExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARecExpression(node: ARecExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARecordFieldExpression(node: ARecordFieldExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAExtendedExprExpression(node: AExtendedExprExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseATypeofExpression(node: ATypeofExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperationCallExpression(node: AOperationCallExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperatorExpression(node: AOperatorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    private fun List<CollectionNode>.findByName(name: String): CollectionNode? {
        return this.find { it.name == name }
    }

    private fun List<CollectionNode>.findEntryByName(name: String): CollectionEntry? {
        return this.find {
            it.elements.find { entry -> entry.name == name } != null
        }?.elements?.find { it.name == name }
    }

    private fun List<Expression>.findByName(name: String): Expression? {
        return this.find { (it as? IdentifierExpression)?.name == name }
    }
}