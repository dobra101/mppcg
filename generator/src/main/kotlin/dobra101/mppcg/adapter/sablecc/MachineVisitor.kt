package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.b.Initialization
import dobra101.mppcg.node.b.Invariant
import dobra101.mppcg.node.b.Machine
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate

class MachineVisitor : AbstractVisitor() {
    override var result: Machine? = null

    private var name: String = ""
    private var parameters: List<Expression> = emptyList()
    private var constraints: Predicate? = null
    var sets: List<CollectionNode> = emptyList()
    var constants: List<Expression> = emptyList()
    var concreteConstants: List<Expression> = emptyList()
    private var properties: Predicate? = null
    private var definitions: Predicate? = null
    var variables: List<Expression> = emptyList()
    var concreteVariables: List<Expression> = emptyList()
    private var initialization: Initialization? = null
    private var invariant: Invariant = Invariant()
    private var assertions: List<Predicate> = emptyList()
    private var operations: List<Operation> = emptyList()

    override fun caseAMachineHeader(node: AMachineHeader) {
        name = node.name[0].text // TODO: more than one name?
        parameters = node.parameters.convert()
    }

    override fun caseAConstraintsMachineClause(node: AConstraintsMachineClause) {
        // TODO: split into multiple?
        constraints = node.predicates.convert()
    }

    override fun caseASetsMachineClause(node: ASetsMachineClause) {
        sets = node.setDefinitions.convert()
    }

    override fun caseAAbstractConstantsMachineClause(node: AAbstractConstantsMachineClause) {
        constants = node.identifiers.convert()
    }

    override fun caseAConstantsMachineClause(node: AConstantsMachineClause) {
        concreteConstants = node.identifiers.convert()
    }

    override fun caseAPropertiesMachineClause(node: APropertiesMachineClause) {
        // TODO: split into multiple
        properties = node.predicates.convert()
    }

    override fun caseADefinitionsMachineClause(node: ADefinitionsMachineClause) {
//        TODO("Not implemented caseADefinitionsMachineClause")
//        definitions = node.definitions.convert()
    }

    override fun caseAVariablesMachineClause(node: AVariablesMachineClause) {
        variables = node.identifiers.convert()
    }

    override fun caseAConcreteVariablesMachineClause(node: AConcreteVariablesMachineClause) {
        concreteVariables = node.identifiers.convert()
    }

    override fun caseAInitialisationMachineClause(node: AInitialisationMachineClause) {
        val substitutions = node.substitutions.convert()
        initialization = if (substitutions == null) {
            null
        } else {
            substitutions as? Initialization ?: Initialization(listOf(substitutions))
        }
    }

    override fun caseAInvariantMachineClause(node: AInvariantMachineClause) {
        invariant = Invariant.of(node.predicates.convert())
    }

    override fun caseAAssertionsMachineClause(node: AAssertionsMachineClause) {
        assertions = node.predicates.convert()
    }

    override fun caseAOperationsMachineClause(node: AOperationsMachineClause) {
        operations = node.operations.convert()
    }

    override fun caseASeesMachineClause(node: ASeesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPromotesMachineClause(node: APromotesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAUsesMachineClause(node: AUsesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAIncludesMachineClause(node: AIncludesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAExtendsMachineClause(node: AExtendsMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAImportsMachineClause(node: AImportsMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAFreetypesMachineClause(node: AFreetypesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAValuesMachineClause(node: AValuesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseALocalOperationsMachineClause(node: ALocalOperationsMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAReferencesMachineClause(node: AReferencesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAInvalidOperationsClauseMachineClause(node: AInvalidOperationsClauseMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAExpressionsMachineClause(node: AExpressionsMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAPredicatesMachineClause(node: APredicatesMachineClause) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseEOF(node: EOF) {
        result = Machine(
            name = name,
            parameters = parameters,
            constraints = constraints,
            sets = sets,
            constants = constants,
            concreteConstants = concreteConstants,
            properties = properties,
            definitions = definitions,
            variables = variables,
            concreteVariables = concreteVariables,
            initialization = initialization,
            invariant = invariant,
            assertions = assertions,
            operations = operations
        )
    }
}