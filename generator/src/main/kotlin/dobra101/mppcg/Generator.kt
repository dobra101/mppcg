package dobra101.mppcg

import de.be4.classicalb.core.parser.node.Start
import dobra101.mppcg.adapter.sablecc.convert
import dobra101.mppcg.environment.OutputLanguageEnvironment
import dobra101.mppcg.node.ClassVariables
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.Program
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import java.io.File

class Generator {
    companion object {
        lateinit var environment: OutputLanguageEnvironment
    }

    val identifierReferenceMap: MutableMap<String, MutableList<NamedNode>> = mutableMapOf()

    fun generate(start: Start, outputPath: String): File {
        val program: Program = start.convert()

        program.handleKeywords()

        val result = program.render()

        val file = File("$outputPath${program.name}.${environment.fileExtension}")
        file.createNewFile()
        file.writeText(result.rendered)
        return file
    }

    private fun Program.handleKeywords() {
        val keywords = environment.renderTemplate("keywords")
            .split(",")
            .map { it.trim() }
            .toSet()

        getAllIdentifiers().forEach { it.getAllIdentifiers() }

        // change names of all identifiers
        for (keyword in keywords) {
            if (!identifierReferenceMap.containsKey(keyword)) continue

            var count = 0
            var newName = "${keyword}_0"
            while (keywords.contains(newName) || identifierReferenceMap.keys.contains(newName)) {
                count++
                newName = "${keyword}_$count"
            }

            identifierReferenceMap[keyword]!!.forEach { it.name = newName }
        }
    }

    // TODO: rename
    private fun MPPCGNode.getAllIdentifiers() {
        when (this) {
            is Expression -> this.getIdentifiersFromExpression()
            is Predicate -> this.getIdentifiersFromPredicate()
            is Substitution -> this.getIdentifiersFromSubstitution()
            is Program -> this.getAllIdentifiers()
            is ClassVariables -> variables.forEach { it.getAllIdentifiers() }

            /* B Nodes */
            is Operation -> {
                identifierReferenceMap.putOrAdd(name, this)
                parameters.forEach { it.getAllIdentifiers() }
                returnValues.forEach { it.getAllIdentifiers() }
                body?.getAllIdentifiers()
            }

            is Transition -> {
                identifierReferenceMap.putOrAdd(name, this)
                body.getAllIdentifiers()
            }

            else -> throw RuntimeException("Unknown ${this::class}")
        }
    }

    private fun Expression.getIdentifiersFromExpression() {
        when (this) {
            is AnonymousSetCollectionNode -> elements.forEach { it.getAllIdentifiers() }
            is BinaryExpression -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is EnumCollectionNode -> {
                identifierReferenceMap.putOrAdd(name, this)
                elements.forEach { it.getAllIdentifiers() }
            }

            is EnumEntry -> identifierReferenceMap.putOrAdd(name, this)
            is IdentifierExpression -> identifierReferenceMap.putOrAdd(name, this)
            is IntervalExpression -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is SetCollectionNode -> {
                identifierReferenceMap.putOrAdd(name, this)
                elements.forEach { it.getAllIdentifiers() }
            }

            is SetEntry -> identifierReferenceMap.putOrAdd(name, this)
            is ValueExpression -> {
                // ignore
            }

            /* B Expressions */ // TODO: move
            is Function -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is BinaryCollectionExpression -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is BinaryFunctionExpression -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is BinarySequenceExpression -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is CallFunctionExpression -> {
                expression.getAllIdentifiers()
                parameters.forEach { it.getAllIdentifiers() }
            }

            is ComprehensionSet -> {
                identifiers.forEach { it.getAllIdentifiers() }
                predicates.getAllIdentifiers()
            }

            is ConcreteIdentifierExpression -> {
                identifierReferenceMap.putOrAdd(name, this)
                value.getAllIdentifiers()
            }

            is Couple -> {
                from.getAllIdentifiers()
                to.getAllIdentifiers()
            }

            is InfiniteSet -> {
                // ignore
            }

            is LambdaExpression -> {
                identifiers.forEach { it.getAllIdentifiers() }
                predicate.getAllIdentifiers()
                expression.getAllIdentifiers()
            }

            is Sequence -> elements.forEach { it.getAllIdentifiers() }
            is UnarySequenceExpression -> sequence.getAllIdentifiers()
            is UnaryCollectionExpression -> collection.getAllIdentifiers()
            is UnaryExpression -> value.getAllIdentifiers()
            is UnaryFunctionExpression -> expression.getAllIdentifiers()

            else -> throw RuntimeException("Unknown ${this::class}")
        }
    }

    private fun Predicate.getIdentifiersFromPredicate() {
        when (this) {
            is BinaryPredicate -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is BinaryLogicPredicate -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is UnaryLogicPredicate -> predicate.getAllIdentifiers()
            is ValuePredicate -> {
                // ignore
            }

            /* B Predicates */
            is Invariant -> predicates.forEach { it.getAllIdentifiers() }
            is QuantifierPredicate -> {
                identifier.forEach { it.getAllIdentifiers() }
                predicate.getAllIdentifiers()
                quantification?.getAllIdentifiers()
            }

            else -> throw RuntimeException("Unknown ${this::class}")
        }
    }

    private fun Substitution.getIdentifiersFromSubstitution() {
        when (this) {
            is AssignSubstitution -> {
                left.getAllIdentifiers()
                right.getAllIdentifiers()
            }

            is DeclarationSubstitution -> assignment.getAllIdentifiers()
            is ElseIfSubstitution -> {
                condition.getAllIdentifiers()
                then.getAllIdentifiers()
            }

            is IfSubstitution -> {
                condition.getAllIdentifiers()
                then.getAllIdentifiers()
                elseIf.forEach { it.getAllIdentifiers() }
                elseSubstitution?.getAllIdentifiers()
            }

            is SequenceSubstitution -> substitutions.forEach { it.getAllIdentifiers() }
            is WhileSubstitution -> {
                condition.getAllIdentifiers()
                body.getAllIdentifiers()
            }

            /* B Substitutions */
            is Initialization -> substitutions.forEach { it.getAllIdentifiers() }
            is ParallelSubstitution -> substitutions.forEach { it.getAllIdentifiers() }
            is Precondition -> {
                substitution.getAllIdentifiers()
                predicate.getAllIdentifiers()
            }

            is Select -> {
                condition.getAllIdentifiers()
                then?.getAllIdentifiers()
                whenSubstitution.forEach { it.getAllIdentifiers() }
                elseSubstitution?.getAllIdentifiers()
            }

            else -> throw RuntimeException("Unknown ${this::class}")
        }
    }

}

// TODO: move
fun MutableMap<String, MutableList<NamedNode>>.putOrAdd(key: String, value: NamedNode) {
    val current = get(key) ?: mutableListOf()
    current.add(value)
    this[key] = current
}