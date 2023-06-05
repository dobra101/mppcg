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
import java.util.logging.Logger

// TODO: generate multiple at once
object Launcher {
    private val logger: Logger = Logger.getLogger(Launcher::class.simpleName)
    fun launch(
        lang: Language,
        file: File,
        parser: Parser,
        optimize: Boolean = false,
        benchmark: Boolean = false,
        outputPath: String = "generator/build/generated/",
        minInt: Long = -1,
        maxInt: Long = 4
    ): File {
        fun copyFile(name: String, inputPath: String) {
            val inputFile = File("$inputPath/$name")
            val outputFile = File("$outputPath/$name")
            if (outputFile.exists()) outputFile.delete()
            inputFile.copyTo(outputFile)
        }

        if (!File(outputPath).exists()) File(outputPath).mkdir()

        val start = when (parser) {
            Parser.SableCC -> BParser(file.name).parseFile(file, false)
            Parser.ANTLR -> throw NotImplementedError("No antlr adapter implemented")
        }

        Generator.environment = environmentOf(lang)
        Generator.environment.optimize = optimize
        RuntimeConfig.config = BEnvironmentConfig(minInt, maxInt)
        val generated = Generator().generate(start, outputPath)
        if (lang == Language.PROLOG) {
            generateRunCfg("${outputPath}runCfg.pl")
            val inputResourcePath = "inputLanguage/B/prolog/src/main/resources"
            copyFile("avl.pl", inputResourcePath)
            copyFile("ordsets.pl", inputResourcePath)
            copyFile("btypes.pl", inputResourcePath)
        }

        if (benchmark) {
            if (lang == Language.PROLOG) {
                benchmarkProlog(generated)
            }
        }

        return generated
    }

    fun launch(
        lang: Language,
        file: String,
        parser: Parser,
        optimize: Boolean = false,
        benchmark: Boolean = false,
        outputPath: String = "generator/build/generated/",
        minInt: Long = -1,
        maxInt: Long = 4
    ): File {
        val filename = if (file.endsWith(".mch")) file else "$file.mch"

        val machine = File("build/resources/main/machines/$filename").let {
            if (it.exists()) it else File("build/resources/test/dobra101/mppcg").walk()
                .filter { fn -> fn.name == filename }.first()
        }

        return launch(lang, machine, parser, optimize, benchmark, outputPath, minInt, maxInt)
    }

    fun benchmarkProlog(
        file: File,
        checkDeadlock: Boolean = true,
        checkInvariant: Boolean = true,
        timeout: Long? = null
    ): ProBResult {
        fun copyFile(name: String, inputPath: String, outputPath: String) {
            val inputFile = File("$inputPath/$name")
            val outputFile = File("$outputPath/$name")
            if (outputFile.exists()) outputFile.delete()
            inputFile.copyTo(outputFile)
        }

        val prologResourcesPath = "outputLanguage/prolog/src/main/resources"
        val probPath = "$prologResourcesPath/ProB_Signed/probcli.sh"
        val probArgs =
            "--model-check ${if (checkDeadlock) "" else "-nodead"} ${if (checkInvariant) "" else "-noinv"} ${if (timeout != null) "--timeout $timeout" else "-disable-time-out"} -p OPERATION_REUSE full -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory"

        val probFile = File("$prologResourcesPath/${file.nameWithoutExtension}.P")
        if (probFile.exists()) probFile.delete()
        file.copyTo(probFile)


        val inputResourcePath = "inputLanguage/B/prolog/src/main/resources"
        copyFile("avl.pl", inputResourcePath, prologResourcesPath)
        copyFile("ordsets.pl", inputResourcePath, prologResourcesPath)
        copyFile("btypes.pl", inputResourcePath, prologResourcesPath)
        copyFile("runCfg.pl", "generator/build/generated", prologResourcesPath)

        val cmd = "$probPath $probArgs ${probFile.absolutePath}"
        val process: Process = Runtime.getRuntime().exec(cmd)

        // store pid to kill prob if needed
        val pid = File("build/pid.txt")
        pid.writeText("${process.pid()}")
        logger.info("Started ProB with pid ${process.pid()}")
        val exitValue = process.waitFor()

        val probOutput = process.inputReader().readText()
        process.inputReader().close()

        if (exitValue != 0) {
            val error = process.errorReader().readText()
            process.errorReader().close()
            val msg = "$probOutput\n$error"
            probFile.deleteOnExit()
            throw RuntimeException(msg)
        }

        // TODO: store results
        val proBResult = ProBResultAnalyser.analyze(probOutput)
        println(proBResult)

        probFile.deleteOnExit()
        pid.deleteOnExit()
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

private fun generateRunCfg(path: String) {
    val file = File(path)
    file.createNewFile()

    val rendered = Generator.environment.renderTemplate(
        "runCfg", mapOf(
            "maxInt" to (RuntimeConfig.config as BEnvironmentConfig).maxInteger,
            "minInt" to (RuntimeConfig.config as BEnvironmentConfig).minInteger
        )
    )

    file.writeText(rendered)
}