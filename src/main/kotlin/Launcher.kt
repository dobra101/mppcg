import dobra101.mppcg.environment.Language
import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.cli.default
import kotlinx.cli.required
import java.io.File

fun main(args: Array<String>) {
    val argParser = ArgParser("generator")
    val lang by argParser.option(ArgType.Choice<Language>(), description = "Language").required()
    val file by argParser.option(ArgType.String, description = "File").required()
    val parser by argParser.option(ArgType.Choice<Parser>(), description = "Parser").default(Parser.SableCC)
    val optimize by argParser.option(ArgType.Boolean, description = "Optimization").default(false)

    argParser.parse(args)

    val filename = if (file.endsWith(".mch")) file else "$file.mch"
    val machine = File("build/resources/main/machines/$filename")
}

enum class Parser {
    SableCC,
    ANTLR
}