package dobra101.mppcg

import de.be4.classicalb.core.parser.BParser
import dobra101.mppcg.environment.*
import dobra101.mppcg.prob.ProBResult
import dobra101.mppcg.prob.ProBResultAnalyser
import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.cli.default
import kotlinx.cli.required
import java.io.File

// TODO: generate multiple at once
object Launcher {
    fun launch(lang: Language, file: String, parser: Parser, optimize: Boolean, benchmark: Boolean): File {
        val filename = if (file.endsWith(".mch")) file else "$file.mch"
        val machine = File("build/resources/main/machines/$filename")
        val start = when (parser) {
            Parser.SableCC -> BParser(machine.name).parseFile(machine, false)
            Parser.ANTLR -> throw NotImplementedError("No antlr adapter implemented")
        }

        Generator.environment = environmentOf(lang)
        Generator.environment.optimize = optimize
        val generated = Generator().generate(start)

        if (benchmark) {
            if (lang == Language.PROLOG) {
                benchmarkProlog(generated)
            }
        }

        return generated
    }

    fun benchmarkProlog(file: File): ProBResult {
        val prologResourcesPath = "prolog/src/main/resources"
        val probPath = "$prologResourcesPath/ProB_Signed/probcli.sh"
        val probArgs =
            "--model-check -disable-time-out -p OPERATION_REUSE full -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory"

        val probFile = File("$prologResourcesPath/${file.nameWithoutExtension}.P")

        if (probFile.exists()) probFile.delete()
        file.copyTo(probFile)

        val cmd = "$probPath $probArgs ${probFile.absolutePath}"
        val process: Process = Runtime.getRuntime().exec(cmd)
        val exitValue = process.waitFor()

        val probOutput = process.inputReader().readText()

        if (exitValue != 0) {
            val error = process.errorReader().readText()
            val msg = "$probOutput\n$error"
            probFile.deleteOnExit()
            throw RuntimeException(msg)
        }

        // TODO: store results
        val proBResult = ProBResultAnalyser.analyze(probOutput)
        println(proBResult)

        probFile.deleteOnExit()
        return proBResult
    }
}

fun main(args: Array<String>) {
    val argParser = ArgParser("generator")
    val lang by argParser.option(ArgType.Choice<Language>(), description = "Language").required()
    val file by argParser.option(ArgType.String, description = "File").required()
    val parser by argParser.option(ArgType.Choice<Parser>(), description = "Parser").default(Parser.SableCC)
    val optimize by argParser.option(ArgType.Boolean, description = "Optimization").default(false)
    val benchmark by argParser.option(ArgType.Boolean, description = "Run Benchmarks").default(false)

    argParser.parse(args)

    Launcher.launch(lang, file, parser, optimize, benchmark)
}

private fun environmentOf(language: Language): OutputLanguageEnvironment {
    return when (language) {
        Language.JAVA -> JavaOutputEnvironment()
        Language.PROLOG -> PrologOutputEnvironment()
    }
}

enum class Parser {
    SableCC,
    ANTLR
}