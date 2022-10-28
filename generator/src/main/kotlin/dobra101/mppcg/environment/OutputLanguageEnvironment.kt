package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.MPPCGNode
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.BinaryPredicate
import dobra101.mppcg.node.predicate.LogicPredicate
import dobra101.mppcg.node.predicate.Predicate
import dobra101.mppcg.node.substitution.AssignSubstitution
import dobra101.mppcg.node.substitution.Substitution
import org.stringtemplate.v4.STGroup
import org.stringtemplate.v4.STGroupFile

/**
 * Contains the rendering functions for each node of an intermediate code representation.
 * Therefore, each node's render()-method is delegated to the correct renderSelf()-method.
 *
 * Collects also the StringTemplate-files and builds a STGroup which can be used to fill the templates
 * using the template name and a map of arguments.
 */
abstract class OutputLanguageEnvironment : EnvironmentUtils(), BEnvironment {
    var optimize: Boolean = true

    abstract val fileExtension: String
    abstract val templateDir: String // TODO: as file
    private val group: STGroup by lazy { importTemplates() } // prevents templateDir from being null

    /* Expression */
    abstract fun BinaryExpression.renderSelf(): RenderResult
    abstract fun EnumCollectionNode.renderSelf(): RenderResult
    abstract fun EnumEntry.renderSelf(): RenderResult
    abstract fun IdentifierExpression.renderSelf(): RenderResult
    abstract fun IntervalExpression.renderSelf(): RenderResult
    abstract fun SetCollectionNode.renderSelf(): RenderResult
    abstract fun SetEntry.renderSelf(): RenderResult
    abstract fun ValueExpression.renderSelf(): RenderResult

    /* Predicate */
    abstract fun BinaryPredicate.renderSelf(): RenderResult
    abstract fun LogicPredicate.renderSelf(): RenderResult

    /* Substitution */
    abstract fun AssignSubstitution.renderSelf(): RenderResult

    fun call(node: MPPCGNode): RenderResult {
        return when (node) {
            is Expression -> callExpression(node)
            is Predicate -> callPredicate(node)
            is Substitution -> callSubstitution(node)

            /* B Nodes */
            is Machine -> node.renderSelf()
            is Operation -> node.renderSelf()
            is Transition -> node.renderSelf()

            else -> throw EnvironmentException("Unknown ${node::class}")
        }
    }

    private fun callExpression(node: Expression): RenderResult {
        return when (node) {
            is BinaryExpression -> node.renderSelf()
            is EnumCollectionNode -> node.renderSelf()
            is EnumEntry -> node.renderSelf()
            is IdentifierExpression -> node.renderSelf()
            is IntervalExpression -> node.renderSelf()
            is SetCollectionNode -> node.renderSelf()
            is SetEntry -> node.renderSelf()
            is ValueExpression -> node.renderSelf()

            /* B Expressions */
            is Function -> node.renderSelf()

            else -> throw EnvironmentException("Unknown ${node::class}")
        }
    }

    private fun callPredicate(node: Predicate): RenderResult {
        return when (node) {
            is BinaryPredicate -> node.renderSelf()
            is LogicPredicate -> node.renderSelf()

            /* B Predicates */
            is Invariant -> node.renderSelf()

            else -> throw EnvironmentException("Unknown ${node::class}")
        }
    }

    private fun callSubstitution(node: Substitution): RenderResult {
        return when (node) {
            is AssignSubstitution -> node.renderSelf()

            /* B Substitutions */
            is Initialization -> node.renderSelf()
            is Precondition -> node.renderSelf()
            is Select -> node.renderSelf()

            else -> throw EnvironmentException("Unknown ${node::class}")
        }
    }

    private fun importTemplates(): STGroup {
        val st = STGroupFile("$templateDir/b_templates.stg")
        st.importTemplates(STGroupFile("$templateDir/expressions.stg"))
        st.importTemplates(STGroupFile("$templateDir/predicates.stg"))
        st.importTemplates(STGroupFile("$templateDir/substitutions.stg"))
        st.importTemplates(STGroupFile("$templateDir/optimizer.stg"))
        return st
    }

    /**
     * Fills a StringTemplate template of the given name with the given arguments and returns the rendered string.
     *
     * @param templateName The name of the template
     * @param map The arguments as key (name) - value pairs
     * @return The rendered string
     */
    fun stRender(templateName: String, map: Map<String, Any?> = emptyMap()): String {
        val st = group.getInstanceOf(templateName)
        map
            .filterValues { it != null }
            .forEach {
                st.add(it.key, (it.value as? RenderResult)?.rendered ?: it.value)
            }
        return st.render()
    }
}