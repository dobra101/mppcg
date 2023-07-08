package dobra101.mppcg

import de.be4.classicalb.core.parser.node.Start
import dobra101.mppcg.adapter.sablecc.convert
import dobra101.mppcg.environment.OutputLanguageEnvironment
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.NamedNode
import dobra101.mppcg.node.Program
import dobra101.mppcg.node.b.ConcreteIdentifierExpression
import java.io.File
import kotlin.reflect.KVisibility
import kotlin.reflect.full.isSubtypeOf
import kotlin.reflect.full.memberProperties
import kotlin.reflect.typeOf

class Generator {
    companion object {
        lateinit var environment: OutputLanguageEnvironment
    }

    private val identifierReferenceMap: MutableMap<String, MutableList<NamedNode>> = mutableMapOf()

    /**
     * Converts the given AST to intermediate code, executes the preprocessing and generates the code.
     *
     * Note: The output language and environment need to be configured before calling this method.
     *
     * @param start The root AST node
     * @param outputPath The output path where the generated file is stored
     *
     * @return The file containing the generated code
     */
    fun generate(start: Start, outputPath: String): File {
        val program: Program = start.convert()

        TypeInference().infereTypes(program)

        program.handleKeywords()

        val result = program.render()

        val file = File("$outputPath${program.name}.${environment.fileExtension}")
        file.createNewFile()
        file.writeText(result.rendered)
        return file
    }

    /**
     * Collects all [NamedNode]s via reflection and replaces keywords by suffixed, modified versions.
     *
     * @receiver The root node
     */
    private fun Program.handleKeywords() {
        val keywords = environment.renderTemplate("keywords")
            .split(",")
            .map { it.trim() }
            .toSet()

        collectAllIdentifiers()

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

    /**
     * Traverses all child nodes and collects [NamedNode]s in the identifierReferenceMap.
     */
    private fun MPPCGNode.collectAllIdentifiers() {
        val mppcgnode = typeOf<MPPCGNode?>()
        val collection = typeOf<Collection<MPPCGNode>>()
        // get all class fields ...
        this::class.memberProperties.forEach { kp ->
            if (kp.visibility == KVisibility.PRIVATE) return@forEach
            if (kp.call(this) == this || (this is ConcreteIdentifierExpression && kp.name == "node")) return@forEach

            // if current node is NamedNode and current field is named "name"
            if (this is NamedNode && kp.name == "name") {
                // ... add this node to the map
                identifierReferenceMap.putOrAdd(kp.call(this) as String, this as NamedNode)
            } else if (kp.returnType.isSubtypeOf(collection)) {
                (kp.call(this) as Collection<*>).forEach { entry -> (entry as MPPCGNode).collectAllIdentifiers() }
            } else if (kp.returnType.isSubtypeOf(mppcgnode)) {
                kp.call(this)?.let { inst -> (inst as MPPCGNode).collectAllIdentifiers() }
            }
        }
    }
}

private fun MutableMap<String, MutableList<NamedNode>>.putOrAdd(key: String, value: NamedNode) {
    val current = get(key) ?: mutableListOf()
    current.add(value)
    this[key] = current
}