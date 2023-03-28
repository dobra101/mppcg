package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.CallFunctionExpression
import dobra101.mppcg.node.b.CallFunctionOperator
import dobra101.mppcg.node.b.Precondition
import dobra101.mppcg.node.b.Select
import dobra101.mppcg.node.expression.IdentifierExpression
import dobra101.mppcg.node.substitution.*

class SubstitutionVisitor : AbstractVisitor() {

    override var result: Substitution? = null

    override fun caseAAssignSubstitution(node: AAssignSubstitution) {
        val left = node.lhsExpression.convert().toMutableList()
        val right = node.rhsExpressions.convert()
        val assignments = mutableListOf<Substitution>()

        for (i: Int in left.indices) {
            val rightType = right[i].type

            val index = machineVisitor.variables.indexOf(left[i])
            if (index != -1) {
                // left is variable
                left[i] = machineVisitor.variables[index]
            }

            // left type is null and right has type
            left[i].type = left[i].type ?: rightType
            if (left[i] is CallFunctionExpression) {
                (left[i] as CallFunctionExpression).operator = CallFunctionOperator.SET
            }
            val assign = AssignSubstitution(left[i], right[i])
            if (rightType == null) {
                assignments.add(assign)
                continue
            }

            // TODO: refactor
            if (assign.left.type!!::class != rightType::class) {
                when {
                    assign.left.type is TypeNumber && rightType is TypeInteger -> {}
                    assign.left.type is TypeNumber && rightType is TypeNatural -> {}
                    assign.left.type is TypeNumber && rightType is TypeNatural1 -> {}
                    assign.left.type is TypeAnonymousCollection && rightType is TypeCollection && assign.left.type == rightType -> {}
                    assign.left.type is TypeSet && rightType is TypeCollection -> {}
                    assign.right.type is TypeAnonymousCollection -> {}
                    rightType is TypeSequence -> {} // TODO: fix
                    else -> {
                        println(assign)
                        throw InvalidTypeException("Types ${assign.left.type} and $rightType do not match.")
                    }
                }
            }

            // set return value
            if (OperationVisitor.returnValues.contains(assign.left)) {
                OperationVisitor.operationType = assign.left.type!!
            }

            // TODO: refactor
            if (left[i] is IdentifierExpression && !machineVisitor.knownIdentifier().contains(left[i])) {
                assignments.add(DeclarationSubstitution(assign.left.type!!, assign))
            } else {
                assignments.add(assign)
            }

            machineVisitor.recognize(assign.left)
        }

        result = if (assignments.size == 1) assignments[0] else ParallelSubstitution(assignments)
    }

    override fun caseAPreconditionSubstitution(node: APreconditionSubstitution) {
        machineVisitor.currentScope = Scope(machineVisitor.currentScope)
        result = Precondition(node.substitution.convert(), node.predicate.convert()!!)
        machineVisitor.currentScope = machineVisitor.currentScope.parent!!
    }

    override fun caseASelectSubstitution(node: ASelectSubstitution) {
        result = Select(
            condition = node.condition.convert()!!,
            then = node.then.convert(),
            whenSubstitution = node.whenSubstitutions.convert(),
            elseSubstitution = node.`else`.convertOrNull()
        )
    }

    override fun caseASelectWhenSubstitution(node: ASelectWhenSubstitution) {
        // TODO: add also to caseAMemberPredicate
        TODO("Not implemented caseASelectWhenSubstitution")
    }

    override fun caseASequenceSubstitution(node: ASequenceSubstitution) {
        result = SequenceSubstitution(node.substitutions.mapNotNull { it.convert() })
    }

    override fun caseABlockSubstitution(node: ABlockSubstitution) {
        result = node.substitution.convert()
    }

    override fun caseASkipSubstitution(node: ASkipSubstitution) {
        // do nothing
        result = null
    }

    override fun caseAAssertionSubstitution(node: AAssertionSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAChoiceSubstitution(node: AChoiceSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAChoiceOrSubstitution(node: AChoiceOrSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIfSubstitution(node: AIfSubstitution) {
        result = IfSubstitution(
            node.condition.convert()!!,
            node.then.convert()!!,
            node.elsifSubstitutions.convert(),
            node.`else`.convert()
        )
    }

    override fun caseAIfElsifSubstitution(node: AIfElsifSubstitution) {
        result = ElseIfSubstitution(node.condition.convert()!!, node.thenSubstitution.convert()!!)
    }

    override fun caseACaseSubstitution(node: ACaseSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseACaseOrSubstitution(node: ACaseOrSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAAnySubstitution(node: AAnySubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALetSubstitution(node: ALetSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABecomesElementOfSubstitution(node: ABecomesElementOfSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseABecomesSuchSubstitution(node: ABecomesSuchSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAVarSubstitution(node: AVarSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFuncOpSubstitution(node: AFuncOpSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOpSubstitution(node: AOpSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperationCallSubstitution(node: AOperationCallSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAWhileSubstitution(node: AWhileSubstitution) {
        // TODO: add variant and invariant
        result = WhileSubstitution(node.condition.convert()!!, node.doSubst.convert()!!)
    }

    override fun caseAParallelSubstitution(node: AParallelSubstitution) {
        result = ParallelSubstitution(node.substitutions.convert())
    }

    override fun caseADefinitionSubstitution(node: ADefinitionSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInvalidSubstitution(node: AInvalidSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAForallSubMessageSubstitution(node: AForallSubMessageSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseARuleFailSubSubstitution(node: ARuleFailSubSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAForLoopSubstitution(node: AForLoopSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAOperatorSubstitution(node: AOperatorSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADefineSubstitution(node: ADefineSubstitution) {
        TODO("Not implemented ${node::class.simpleName}")
    }
}