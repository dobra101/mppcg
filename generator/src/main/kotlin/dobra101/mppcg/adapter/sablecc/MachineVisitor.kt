package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.*
import dobra101.mppcg.node.b.Initialization
import dobra101.mppcg.node.b.Invariant
import dobra101.mppcg.node.b.Machine
import dobra101.mppcg.node.b.Operation
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.collection.EnumCollectionNode
import dobra101.mppcg.node.collection.EnumEntry
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.predicate.Predicate

class MachineVisitor : AbstractVisitor() {
    override var result: Machine? = null

    private var name: String = ""
    private var parameters: List<Expression> = emptyList()
    private var constraints: Predicate? = null
    private var sets: List<CollectionNode> = emptyList()
    private var constants: List<Expression> = emptyList()
    private var concreteConstants: List<Expression> = emptyList()
    private var properties: Predicate? = null
    private var definitions: Predicate? = null
    private var variables: List<Expression> = emptyList()
    private var concreteVariables: List<Expression> = emptyList()
    private var initialization: Initialization? = null
    private var invariant: Predicate? = null
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
        // TODO: dont convert every set to enum
        val collectionNodes = node.setDefinitions.convert()
        sets = collectionNodes.map {
            EnumCollectionNode(
                name = it.name,
                elements = it.elements.map {elem ->
                    EnumEntry(
                        name = elem.name,
                        enum = elem.collection
                    )
                }
            )
        }
    }

    override fun caseAAbstractConstantsMachineClause(node: AAbstractConstantsMachineClause) {
        constants = node.identifiers.convert()
    }

    override fun caseAConstantsMachineClause(node: AConstantsMachineClause) {
        concreteConstants = node.identifiers.convert()
    }

    override fun caseAPropertiesMachineClause(node: APropertiesMachineClause) {
        properties = node.predicates.convert()
    }

    override fun caseADefinitionsMachineClause(node: ADefinitionsMachineClause) {
        TODO("Not implemented caseADefinitionsMachineClause")
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
        // TODO: split into multiple?
        val predicates = node.predicates.convert()
        invariant = if (predicates != null) Invariant(predicates) else null
    }

    override fun caseAAssertionsMachineClause(node: AAssertionsMachineClause) {
        assertions = node.predicates.convert()
    }

    override fun caseAOperationsMachineClause(node: AOperationsMachineClause) {
        operations = node.operations.convert()
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