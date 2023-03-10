package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.b.Sequence
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*

class ExpressionVisitor : AbstractVisitor() {

    override var result: Expression? = null
        set(value) {
            AbstractVisitor.result = value
            field = value
        }

    override fun caseTIdentifierLiteral(node: TIdentifierLiteral) {
        val parent = node.parent().parent() // first "parent" is AIdentifierExpression

        if (parent is AEnumeratedSetSet) {
            // TODO: when more than one identifier?

            // a, b, c = 1, 2, 3
            val enumName = parent.identifier[0].text
            result = IdentifierExpression(name = node.text, type = TypeCollection(CollectionType.Enum, enumName))
            return
        }

        val identifierExpression = IdentifierExpression(name = node.text)

        result = machineVisitor.variables.findByName(node.text)
        result?.let { return }

        result = machineVisitor.concreteConstants.findByName(node.text)
        result?.let { return }

        result = machineVisitor.constants.findByName(node.text)
        result?.let { return }

        result = machineVisitor.sets.findByName(node.text)?.copy() ?: machineVisitor.sets.findEntryByName(node.text)
                ?: identifierExpression
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

    override fun caseAMinusOrSetSubtractExpression(node: AMinusOrSetSubtractExpression) {
        val left = node.left.convert()!!.setParameterIfCollection()
        val right = node.right.convert()!!.setParameterIfCollection()
        result =
                // TODO: refactor classes
            if (left is CollectionNode || right is CollectionNode || left is AnonymousCollectionNode || right is AnonymousSetCollectionNode) {
                BinaryCollectionExpression(left, right, BinaryCollectionOperator.SUBTRACTION)
            } else {
                BinaryExpression(left, right, BinaryExpressionOperator.MINUS)
            }
    }

    override fun caseATotalInjectionExpression(node: ATotalInjectionExpression) {
        if (node.parent() is AMemberPredicate && AbstractVisitor.result is Expression) {
            // is type info
            (AbstractVisitor.result as Expression).type = TypeFunction(FunctionType.TOTAL)
        }
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.INJECTION
        )
    }

    override fun caseAPartialInjectionExpression(node: APartialInjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.INJECTION
        )
    }

    override fun caseATotalBijectionExpression(node: ATotalBijectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.BIJECTION
        )
    }

    override fun caseAPartialBijectionExpression(node: APartialBijectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.BIJECTION
        )
    }

    override fun caseATotalSurjectionExpression(node: ATotalSurjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.SURJECTION
        )
    }

    override fun caseAPartialSurjectionExpression(node: APartialSurjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.SURJECTION
        )
    }

    override fun caseATotalFunctionExpression(node: ATotalFunctionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.TOTAL),
            mapType = FunctionMapType.FUNCTION
        )
    }

    override fun caseAPartialFunctionExpression(node: APartialFunctionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            type = TypeFunction(FunctionType.PARTIAL),
            mapType = FunctionMapType.FUNCTION
        )
    }

    override fun caseAIntervalExpression(node: AIntervalExpression) {
        result = IntervalExpression(node.leftBorder.convert()!!, node.rightBorder.convert()!!)
    }

    override fun caseAIntegerSetExpression(node: AIntegerSetExpression) {
        if (node.parent() is AMemberPredicate && AbstractVisitor.result is Expression) {
            // is type info
            (AbstractVisitor.result as Expression).type = TypeInteger()
        }
        result = InfiniteSet(TypeInteger())
    }

    override fun caseANaturalSetExpression(node: ANaturalSetExpression) {
        if (node.parent() is AMemberPredicate && AbstractVisitor.result is Expression) {
            // is type info
            (AbstractVisitor.result as Expression).type = TypeNatural()
        }
        result = InfiniteSet(TypeNatural())
    }

    override fun caseASetExtensionExpression(node: ASetExtensionExpression) {
        val expressions = node.expressions.convert()
        result = AnonymousSetCollectionNode(expressions)
    }

    override fun caseAEmptySetExpression(node: AEmptySetExpression) {
        result = AnonymousSetCollectionNode()
    }

    override fun caseAReverseExpression(node: AReverseExpression) {
        result = UnaryFunctionExpression(node.expression.convert()!!, UnaryFunctionOperator.REVERSE)
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
        result = ValueExpression("", type = TypeBoolean(BooleanValue.TRUE))
    }

    override fun caseABooleanFalseExpression(node: ABooleanFalseExpression) {
        result = ValueExpression("", type = TypeBoolean(BooleanValue.FALSE))
    }

    override fun caseAIntegerExpression(node: AIntegerExpression) {
        // not needed -> see caseTIntegerLiteral
        super.caseAIntegerExpression(node)
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
        trySetPreviousResultType(TypeReal())
        result = InfiniteSet(TypeReal())
    }

    override fun caseAFloatSetExpression(node: AFloatSetExpression) {
        trySetPreviousResultType(TypeFloat())
        result = InfiniteSet(TypeFloat())
    }

    override fun caseANatural1SetExpression(node: ANatural1SetExpression) {
        trySetPreviousResultType(TypeNatural1())
        result = InfiniteSet(TypeNatural1())
    }

    override fun caseANatSetExpression(node: ANatSetExpression) {
        trySetPreviousResultType(TypeNatural())
        result = InfiniteSet(TypeNatural())
    }

    override fun caseANat1SetExpression(node: ANat1SetExpression) {
        trySetPreviousResultType(TypeNatural1())
        result = InfiniteSet(TypeNatural1())
    }

    override fun caseAIntSetExpression(node: AIntSetExpression) {
        trySetPreviousResultType(TypeInteger())
        result = InfiniteSet(TypeInteger())
    }

    override fun caseABoolSetExpression(node: ABoolSetExpression) {
        trySetPreviousResultType(TypeBoolean())
        result = AnonymousSetCollectionNode(
            listOf(
                ValueExpression("", TypeBoolean(BooleanValue.TRUE)),
                ValueExpression("", TypeBoolean(BooleanValue.FALSE))
            )
        )
    }

    override fun caseAStringSetExpression(node: AStringSetExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAConvertBoolExpression(node: AConvertBoolExpression) {
        result = UnaryExpression(node.predicate.convert()!!, UnaryExpressionOperator.CONVERT_BOOLEAN)
    }

    override fun caseAUnaryMinusExpression(node: AUnaryMinusExpression) {
        result = UnaryExpression(node.expression.convert()!!, UnaryExpressionOperator.MINUS)
    }

    override fun caseAMultiplicationExpression(node: AMultiplicationExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACartesianProductExpression(node: ACartesianProductExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAMultOrCartExpression(node: AMultOrCartExpression) {
        // TODO: implement cart
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.MULT)
    }

    override fun caseADivExpression(node: ADivExpression) {
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.DIV)
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
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.MOD)
    }

    override fun caseAPowerOfExpression(node: APowerOfExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASuccessorExpression(node: ASuccessorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
//        result = UnaryExpression(
//            UnaryExpressionOperator.SUCC
//        )
    }

    override fun caseAPredecessorExpression(node: APredecessorExpression) {
        TODO("Not implemented ${node::class.simpleName}")
//        result = UnaryExpression(
//            node.convert()!!,
//            UnaryExpressionOperator.PRED
//        )
    }

    override fun caseAMaxExpression(node: AMaxExpression) {
        result = UnaryCollectionExpression(
            node.expression.convert()!!.setParameterIfCollection(),
            UnaryCollectionOperator.MAX
        )
    }

    override fun caseAMinExpression(node: AMinExpression) {
        result = UnaryCollectionExpression(
            node.expression.convert()!!.setParameterIfCollection(),
            UnaryCollectionOperator.MIN
        )
    }

    override fun caseACardExpression(node: ACardExpression) {
        result = UnaryCollectionExpression(
            node.expression.convert()!!.setParameterIfCollection(),
            UnaryCollectionOperator.CARD
        )
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
        val list = node.list.convert()
        if (list.size != 2) {
            throw VisitorException("Cannot create Couple from $list")
        }

        result = Couple(from = list[0], to = list[1])
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
        if (node.parent() is AMemberPredicate && AbstractVisitor.result is Expression) {
            // is type info
            (AbstractVisitor.result as Expression).type = TypeSet(type = TypeInteger()) // TODO: can be anything
        }
        result = UnaryCollectionExpression(
            node.expression.convert()!!.setParameterIfCollection(),
            UnaryCollectionOperator.POW
        )
    }

    override fun caseAPow1SubsetExpression(node: APow1SubsetExpression) {
        result = UnaryCollectionExpression(
            node.expression.convert()!!.setParameterIfCollection(),
            UnaryCollectionOperator.POW1
        )
        println("POW1: ${result!!.type}")
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
        result = BinaryCollectionExpression(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryCollectionOperator.UNION
        )
    }

    override fun caseAIntersectionExpression(node: AIntersectionExpression) {
        result = BinaryCollectionExpression(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryCollectionOperator.INTERSECTION
        )
    }

    override fun caseASetSubtractionExpression(node: ASetSubtractionExpression) {
        result = BinaryCollectionExpression(
            node.left.convert()!!.setParameterIfCollection(),
            node.right.convert()!!.setParameterIfCollection(),
            BinaryCollectionOperator.SUBTRACTION
        )
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
        result = node.expression.convert()!! // TODO: thats it?
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
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.FORWARD_COMPOSITION)
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
        result = UnaryFunctionExpression(node.expression.convert()!!, UnaryFunctionOperator.DOMAIN)
    }

    override fun caseARangeExpression(node: ARangeExpression) {
        result = UnaryFunctionExpression(node.expression.convert()!!, UnaryFunctionOperator.RANGE)
    }

    override fun caseAImageExpression(node: AImageExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.IMAGE)
    }

    override fun caseADomainRestrictionExpression(node: ADomainRestrictionExpression) {
        result = BinaryFunctionExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinaryFunctionOperator.DOMAIN_RESTRICTION
        )
    }

    override fun caseADomainSubtractionExpression(node: ADomainSubtractionExpression) {
        result = BinaryFunctionExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinaryFunctionOperator.DOMAIN_SUBTRACTION
        )
    }

    override fun caseARangeRestrictionExpression(node: ARangeRestrictionExpression) {
        result = BinaryFunctionExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinaryFunctionOperator.RANGE_RESTRICTION
        )
    }

    override fun caseARangeSubtractionExpression(node: ARangeSubtractionExpression) {
        result = BinaryFunctionExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinaryFunctionOperator.RANGE_SUBTRACTION
        )
    }

    override fun caseAOverwriteExpression(node: AOverwriteExpression) {
        result =
            BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.OVERWRITE)
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
        result = LambdaExpression(node.identifiers.convert(), node.predicate.convert()!!, node.expression.convert()!!)
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
        result = Sequence(sequenceType = node.expression.convert()!!.type)
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
        result = Sequence()
    }

    override fun caseASequenceExtensionExpression(node: ASequenceExtensionExpression) {
        result = Sequence(node.expression.convert())
    }

    override fun caseASizeExpression(node: ASizeExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFirstExpression(node: AFirstExpression) {
        result = UnarySequenceExpression(node.expression.convert()!!, UnarySequenceExpressionOperator.FIRST)
    }

    override fun caseALastExpression(node: ALastExpression) {
        result = UnarySequenceExpression(node.expression.convert()!!, UnarySequenceExpressionOperator.LAST)
    }

    override fun caseAFrontExpression(node: AFrontExpression) {
        result = UnarySequenceExpression(node.expression.convert()!!, UnarySequenceExpressionOperator.FRONT)
    }

    override fun caseATailExpression(node: ATailExpression) {
        result = UnarySequenceExpression(node.expression.convert()!!, UnarySequenceExpressionOperator.TAIL)
    }

    override fun caseARevExpression(node: ARevExpression) {
        result = UnarySequenceExpression(node.expression.convert()!!, UnarySequenceExpressionOperator.REVERSE)
    }

    override fun caseAConcatExpression(node: AConcatExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInsertFrontExpression(node: AInsertFrontExpression) {
        result = BinarySequenceExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinarySequenceExpressionOperator.PREPEND
        )
    }

    override fun caseAInsertTailExpression(node: AInsertTailExpression) {
        result = BinarySequenceExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinarySequenceExpressionOperator.APPEND
        )
    }

    override fun caseARestrictFrontExpression(node: ARestrictFrontExpression) {
        result = BinarySequenceExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinarySequenceExpressionOperator.RESTRICT_FRONT
        )
    }

    override fun caseARestrictTailExpression(node: ARestrictTailExpression) {
        result = BinarySequenceExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinarySequenceExpressionOperator.RESTRICT_TAIL
        )
    }

    override fun caseAGeneralConcatExpression(node: AGeneralConcatExpression) {
        println(node.expression.convert())
        TODO()
//        result = BinarySequenceExpression(
//            node.expression
//            node.left.convert()!!,
//            node.right.convert()!!,
//            BinarySequenceExpressionOperator.CONCAT
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
        return find { it.name == name }
    }

    private fun List<CollectionNode>.findEntryByName(name: String): CollectionEntry? {
        return find {
            it.elements.find { entry -> entry.name == name } != null
        }?.elements?.find { it.name == name }
    }

    private fun List<Expression>.findByName(name: String): Expression? {
        return find { (it as? IdentifierExpression)?.name == name }
    }

    private fun trySetPreviousResultType(type: Type) {
        if (AbstractVisitor.result !is Expression) return

        val expr = AbstractVisitor.result as Expression
        if ((expr.type == null || expr.type == type || expr.type is TypeAnonymousCollection)) {
            (AbstractVisitor.result as Expression).type = type
            return
        }
        throw InvalidTypeException("Cannot reassign type of ${AbstractVisitor.result} to $type")
    }
}