package dobra101.mppcg.environment

import org.stringtemplate.v4.STGroup

abstract class LanguageEnvironment : EnvironmentUtils() {
    abstract val fileExtension: String
    abstract val templateDir: String
    private val group: STGroup by lazy { importTemplates() } // prevents templateDir from being null

    private fun importTemplates(): STGroup {
        TODO("Template file import not implemented.")
    }
}