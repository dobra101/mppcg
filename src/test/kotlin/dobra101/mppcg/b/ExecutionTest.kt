package dobra101.mppcg.b

import dobra101.mppcg.Launcher
import dobra101.mppcg.Parser
import dobra101.mppcg.environment.EnvironmentException
import dobra101.mppcg.environment.Language
import io.kotest.assertions.errorCollectorContextElement
import io.kotest.assertions.withClue
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.string.shouldBeEqualIgnoringCase
import org.stringtemplate.v4.STGroupFile
import se.sics.jasper.Query
import se.sics.jasper.SICStus
import se.sics.jasper.SPTerm
import se.sics.jasper.Term
import java.io.File

val outputDir = File("build/executionTests/")

// TODO: refactor: load file in setup file and execute setup file?
class ExecutionTestProlog : ExecutionTest(Language.PROLOG, "prolog.stg", ".pl", runSetup) {
    companion object {
        private var sicstus: SICStus? = null
        private val runSetup = { _: String, file: File, setupFile: File ->
            val sb = StringBuilder()
            val wayMap = hashMapOf<String, Term>()
            // assign here to have the same caller every time
            if (sicstus == null) sicstus = SICStus()
            sicstus!!.load(file.absolutePath)

            val query: Query = sicstus!!.openPrologQuery(setupFile.readText(), wayMap)

            while (query.nextSolution()) {
                wayMap.forEach { (k, v) ->
                    if (k.startsWith("Result_")) {
                        // TODO: not here?
                        // TODO: refactor
                        try {
                            if (v is SPTerm && v.functorName == "set") {
                                val setArg = sicstus!!.newVariable()
                                v.getArg(1, setArg)
                                val value =
                                    if (setArg.isEmptyList) {
                                        "{}"
                                    } else {
                                        (setArg as SPTerm).toTermArray().toList().joinToString(",", "{", "}")
                                    }
                                sb.append("${k.removePrefix("Result_")}=$value")
                            } else {
                                sb.append("${k.removePrefix("Result_")}=$v")
                            }
                        } catch (e: Exception) {
                            sb.append("${k.removePrefix("Result_")}=$v")
                        }
                        sb.append("\n")
                    }
                }
            }
            query.close()
            sb.toString()
        }
    }
}

class ExecutionTestJava : ExecutionTest(Language.JAVA, "java.stg", ".java", runSetup) {
    companion object {
        private val runSetup = { dir: String, file: File, setupFile: File ->
            // TODO: check if btypes.jar exist
            compile(cp = outputDir.path + "/$dir:inputLanguage/B/java/build/libs/btypes.jar", file, setupFile)
            execute(cp = outputDir.path + "/$dir:inputLanguage/B/java/build/libs/btypes.jar", setup = setupFile)
        }

        private fun compile(cp: String, vararg files: File) {
            println("Compile: javac -Xlint:unchecked -cp $cp: ${files.joinToString(" ") { it.path }}")
            val process: Process =
                Runtime.getRuntime().exec("javac -Xlint:unchecked -cp $cp: ${files.joinToString(" ") { it.path }}")
            process.waitFor()
            val error = process.errorReader().readText()
            process.errorReader().close()
            if (error.isNotBlank()) {
                throw RuntimeException(error)
            }
        }

        private fun execute(cp: String, setup: File): String {
            println("Execute: java -cp $cp: ${setup.path}")
            val process: Process = Runtime.getRuntime().exec("java -cp $cp: ${setup.path}")
            process.waitFor()
            val error = process.errorReader().readText()
            process.errorReader().close()
            if (error.isNotBlank()) {
                throw RuntimeException(error)
            }

            val result = process.inputReader().readText()
            process.inputReader().close()

            return result
        }
    }
}

abstract class ExecutionTest(
    language: Language,
    setupFileName: String,
    setupFileExtension: String,
    runSetup: (String, File, File) -> String
) : ExpectSpec({

    if (!outputDir.exists()) outputDir.mkdir()

    // TODO: include deeper nested directories
    val dirs = File("src/test/resources/dobra101/mppcg/execution/").listFiles()
        ?.filter { it.isDirectory }
        ?.associate { dir ->
            val files = dir.walk()
                .filter { it.isFile && it.name.endsWith(".mch") }
                .associateWith { File(it.path.replace(".mch", ".execPath")) }
            dir.name to files
        } ?: emptyMap()

    dirs.forAll { (dir, machines) ->
        context(dir) {
            machines.forAll { (mch, exec) ->
                context(mch.name) {
                    println("Testing machine .(${mch.name}:1)")
                    println("Execution .(${mch.nameWithoutExtension}.execPath:1)")
                    val execution = Execution.of(exec)
                    val setupFile =
                        createSetupFile(dir, setupFileName, setupFileExtension, mch.nameWithoutExtension, execution)

                    listOf(true, false).forAll { optimize ->
                        val expectName = if (optimize) "optimized" else "regular"
                        expect(expectName) {
                            val file = Launcher.launch(
                                lang = language,
                                file = mch,
                                parser = Parser.SableCC,
                                optimize = optimize,
                                benchmark = false,
                                outputPath = "${outputDir.path}/$dir/"
                            )

                            val resultString = runSetup(dir, file, setupFile)
                            val resultMap = string2ResultMap(resultString)
                            for ((key, value) in execution.result) {
                                withClue("Expect ${key.joinToString { it.method }} = $value") {
                                    resultMap[key.map { it.method }] shouldBeEqualIgnoringCase value
                                }
                            }
                        }
                    }
                }
            }
        }
    }
})


private fun createSetupFile(
    dir: String,
    setupFileName: String,
    setupFileExtension: String,
    mchName: String,
    execution: Execution
): File {
    val setup = STGroupFile("src/test/resources/dobra101/mppcg/execution/setup/$setupFileName")

    val methodChains = execution.result.keys.map {
        val methodChain = setup.getInstanceOf("methodChain") ?: throw EnvironmentException("Template 'methodChain' not found")
        methodChain.add("chain", it)
        methodChain.render()
    }.toList()

    val st = setup.getInstanceOf("setup") ?: throw EnvironmentException("Template 'setup' not found")
    st.add("name", mchName)
    st.add("execution", execution.operations)
    st.add("result", methodChains)

    val directory = File("${outputDir.path}/$dir")
    if (!directory.exists()) directory.mkdir()
    val setupFile = File("${outputDir.path}/$dir/${mchName}Setup$setupFileExtension")
    setupFile.writeText(st.render())
    return setupFile
}

private fun string2ResultMap(string: String): Map<List<String>, String> {
    return string.split("\n")
        .filter { it.isNotBlank() }
        .associate {
            val output = it.split("=")
            output[0].split(".").map { s -> s.removeSuffix("()").trim() } to output[1].trim()
        }
}

data class Execution(val operations: List<ExecOperation>, val result: Map<List<ExecOperation>, String>) {
    companion object {
        fun of(file: File): Execution {
            val content = file.readText()
            var operationsProcessed = false
            val operations = mutableListOf<ExecOperation>()
            val result = mutableMapOf<List<ExecOperation>, String>()
            var methodChainList: MutableList<ExecOperation>

            for (line in content.lines()) {
                // TODO: not hardcoded
                if (line.startsWith("***")) {
                    operationsProcessed = true
                    continue
                }

                if (operationsProcessed) {
                    // analyze result expression
                    val expect = line.split("=")
                    val value = expect[1].trim()
                    methodChainList = mutableListOf()
                    val methodChain = expect[0].split("|").map { it.trim() }
                    methodChain.forEach {
                        if (it.startsWith(".")) {
                            val execOperation = ExecOperation(it.removePrefix("."), propertyAccess = true)
                            methodChainList.add(execOperation)
                        } else {
                            methodChainList.add(ExecOperation(it))
                        }
                    }
                    result[methodChainList] = value
                } else {
                    operations.add(ExecOperation(line))
                }
            }

            return Execution(operations, result)
        }
    }
}

data class ExecOperation(val method: String, val propertyAccess: Boolean = false) {
    val parameterized: Boolean = method.endsWith(")") // needed in .stg
}