package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.environment.BEnvironmentConfig
import dobra101.mppcg.environment.RuntimeConfig
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
        result = machineVisitor.knownIdentifier().toList().findByName(node.text)
        result?.let { machineVisitor.recognize(result!!); return } // TODO: this recognize useless?

        result = machineVisitor.sets.findByName(node.text)?.copy() ?: machineVisitor.sets.findEntryByName(node.text)
                ?: IdentifierExpression(name = node.text)
        if (!OperationVisitor.returnValues.contains(result!!)) {
            machineVisitor.recognize(result!!)
        }
    }

    override fun caseTIntegerLiteral(node: TIntegerLiteral) {
        result = ValueExpression(node.text, valueType = MPPCG_Int)
    }

    override fun caseTStringLiteral(node: TStringLiteral) {
        result = ValueExpression(node.text, valueType = MPPCG_String)
    }

    override fun caseTRealLiteral(node: TRealLiteral) {
        result = ValueExpression(node.text, valueType = MPPCG_Real)
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
            if (left is CollectionNode || right is CollectionNode || left is AnonymousCollectionNode) {
                BinaryCollectionExpression(left, right, BinaryCollectionOperator.SUBTRACTION)
            } else {
                BinaryExpression(left, right, BinaryExpressionOperator.MINUS)
            }
    }

    override fun caseATotalInjectionExpression(node: ATotalInjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.TOTAL,
            mapType = FunctionMapType.INJECTION
        )
    }

    override fun caseAPartialInjectionExpression(node: APartialInjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.PARTIAL,
            mapType = FunctionMapType.INJECTION
        )
    }

    override fun caseATotalBijectionExpression(node: ATotalBijectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.TOTAL,
            mapType = FunctionMapType.BIJECTION
        )
    }

    override fun caseAPartialBijectionExpression(node: APartialBijectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.PARTIAL,
            mapType = FunctionMapType.BIJECTION
        )
    }

    override fun caseATotalSurjectionExpression(node: ATotalSurjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.TOTAL,
            mapType = FunctionMapType.SURJECTION
        )
    }

    override fun caseAPartialSurjectionExpression(node: APartialSurjectionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.PARTIAL,
            mapType = FunctionMapType.SURJECTION
        )
    }

    override fun caseATotalFunctionExpression(node: ATotalFunctionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.TOTAL,
            mapType = FunctionMapType.FUNCTION
        )
    }

    override fun caseAPartialFunctionExpression(node: APartialFunctionExpression) {
        result = Function(
            left = node.left.convert()!!.setParameterIfCollection(),
            right = node.right.convert()!!.setParameterIfCollection(),
            functionType = FunctionType.PARTIAL,
            mapType = FunctionMapType.FUNCTION
        )
    }

    override fun caseAIntervalExpression(node: AIntervalExpression) {
        result = IntervalExpression(node.leftBorder.convert()!!, node.rightBorder.convert()!!)
    }

    override fun caseAIntegerSetExpression(node: AIntegerSetExpression) {
        result = InfiniteSet(MPPCG_Integer)
    }

    override fun caseANaturalSetExpression(node: ANaturalSetExpression) {
        result = InfiniteSet(MPPCG_Natural)
    }

    override fun caseASetExtensionExpression(node: ASetExtensionExpression) {
        val expressions = node.expressions.convert()
        result = AnonymousCollectionNode(expressions)
    }

    override fun caseAEmptySetExpression(node: AEmptySetExpression) {
        result = AnonymousCollectionNode()
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
        result = ValueExpression("true", valueType = MPPCG_Boolean)
    }

    override fun caseABooleanFalseExpression(node: ABooleanFalseExpression) {
        result = ValueExpression("false", valueType = MPPCG_Boolean)
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
        result = ValueExpression(
            value = (RuntimeConfig.config as BEnvironmentConfig).maxInteger.toString(),
            valueType = MPPCG_Int
        )
    }

    override fun caseAMinIntExpression(node: AMinIntExpression) {
        result = ValueExpression(
            value = (RuntimeConfig.config as BEnvironmentConfig).minInteger.toString(),
            valueType = MPPCG_Int
        )
    }

    override fun caseARealSetExpression(node: ARealSetExpression) {
        result = InfiniteSet(MPPCG_Real)
    }

    override fun caseAFloatSetExpression(node: AFloatSetExpression) {
        result = InfiniteSet(MPPCG_Float)
    }

    override fun caseANatural1SetExpression(node: ANatural1SetExpression) {
        result = InfiniteSet(MPPCG_Natural1)
    }

    override fun caseANatSetExpression(node: ANatSetExpression) {
        result = InfiniteSet(MPPCG_Nat)
    }

    override fun caseANat1SetExpression(node: ANat1SetExpression) {
        result = InfiniteSet(MPPCG_Nat1)
    }

    override fun caseAIntSetExpression(node: AIntSetExpression) {
        result = InfiniteSet(MPPCG_Int)
    }

    override fun caseABoolSetExpression(node: ABoolSetExpression) {
        result = InfiniteSet(MPPCG_Boolean)
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
        val left = node.left.convert()!!.setParameterIfCollection()
        val right = node.right.convert()!!.setParameterIfCollection()
        result = BinaryExpression(left, right, BinaryExpressionOperator.MULT)
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
        result = BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.POW)
    }

    override fun caseASuccessorExpression(node: ASuccessorExpression) {
        throw VisitorException("Successor Expression should not be reachable")

    }

    override fun caseAPredecessorExpression(node: APredecessorExpression) {
        throw VisitorException("Predecessor Expression should not be reachable")
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
        result = GeneralSumOrProductExpression(
            node.identifiers.convert().map { it as IdentifierExpression },
            node.predicates.convert()!!,
            node.expression.convert()!!,
            SumOrProductOperation.SUM
        )
    }

    override fun caseAGeneralProductExpression(node: AGeneralProductExpression) {
        result = GeneralSumOrProductExpression(
            node.identifiers.convert().map { it as IdentifierExpression },
            node.predicates.convert()!!,
            node.expression.convert()!!,
            SumOrProductOperation.PRODUCT
        )
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
        println(node.left.convert())
        println(node.right.convert())
        //result = UnaryFunctionExpression(node.)
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIdentityExpression(node: AIdentityExpression) {
        result = node.expression.convert()!! // TODO: that's it?
    }

    override fun caseAEventBIdentityExpression(node: AEventBIdentityExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFirstProjectionExpression(node: AFirstProjectionExpression) {
        result = BinaryCollectionExpression(node.exp1.convert()!!, node.exp2.convert()!!, BinaryCollectionOperator.PRJ1)
    }

    override fun caseAEventBFirstProjectionExpression(node: AEventBFirstProjectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBFirstProjectionV2Expression(node: AEventBFirstProjectionV2Expression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseASecondProjectionExpression(node: ASecondProjectionExpression) {
        result = BinaryCollectionExpression(node.exp1.convert()!!, node.exp2.convert()!!, BinaryCollectionOperator.PRJ2)
    }

    override fun caseAEventBSecondProjectionExpression(node: AEventBSecondProjectionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEventBSecondProjectionV2Expression(node: AEventBSecondProjectionV2Expression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACompositionExpression(node: ACompositionExpression) {
        result = BinaryFunctionExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinaryFunctionOperator.FORWARD_COMPOSITION
        )
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
        result =
            BinaryExpression(node.left.convert()!!, node.right.convert()!!, BinaryExpressionOperator.PARALLEL_PRODUCT)
    }

    override fun caseAIterationExpression(node: AIterationExpression) {
        result = BinaryFunctionExpression(node.left.convert()!!, node.right.convert()!!, BinaryFunctionOperator.ITERATE)
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
        val expr = node.expression.convert()
        if (expr !is InfiniteSet) {
            TODO("Not implemented")
        } else {
            result = AnonymousCollectionNode(
                collectionType = TypeOperator(
                    "sequence",
                    types = listOf(expr.setType)
                )
            )

        }
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
        result = BinarySequenceExpression(
            node.left.convert()!!,
            node.right.convert()!!,
            BinarySequenceExpressionOperator.CONCAT
        )
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
        val anonymousSet = node.expression.convert()!!
        if (anonymousSet !is AnonymousCollectionNode) {
            throw VisitorException("Unknown set ${anonymousSet::class} for general concat.")
        }

        result = BinaryCollectionExpression(
            anonymousSet.elements[0],
            AnonymousCollectionNode(anonymousSet.elements.subList(1, anonymousSet.elements.size)),
            BinaryCollectionOperator.CONCAT
        )
    }

    override fun caseADefinitionExpression(node: ADefinitionExpression) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFunctionExpression(node: AFunctionExpression) {
        result = when (node.identifier) {
            is APredecessorExpression -> {
                UnaryExpression(node.parameters[0].convert()!!, UnaryExpressionOperator.PRED)
            }

            is ASuccessorExpression -> {
                UnaryExpression(node.parameters[0].convert()!!, UnaryExpressionOperator.SUCC)
            }

            else -> {
                CallFunctionExpression(node.identifier.convert()!!, node.parameters.convert())
            }
        }
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
        return find {
            (it as? IdentifierExpression)?.name == name
                    || (it as? ConcreteIdentifierExpression)?.name == "c_$name" // TODO: prefix not hardcoded
        }
    }
}