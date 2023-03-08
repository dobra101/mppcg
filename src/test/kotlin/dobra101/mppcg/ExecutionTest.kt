package dobra101.mppcg

import dobra101.mppcg.environment.EnvironmentException
import dobra101.mppcg.environment.Language
import io.kotest.assertions.withClue
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.shouldBe
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
        private val runSetup = { file: File, setupFile: File ->
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
        private val runSetup = { file: File, setupFile: File ->
            compile(file, setupFile)
            execute(cp = outputDir.path, setup = setupFile)
        }

        private fun compile(vararg files: File) {
            println("Compile: javac ${files.joinToString(" ") { it.path }}")
            val process: Process = Runtime.getRuntime().exec("javac ${files.joinToString(" ") { it.path }}")
            process.waitFor()
            val error = process.errorReader().readText()
            process.errorReader().close()
            if (error.isNotBlank()) {
                throw RuntimeException(error)
            }
        }

        private fun execute(cp: String, setup: File): String {
            println("Execute: java -cp $cp ${setup.path}")
            val process: Process = Runtime.getRuntime().exec("java -cp $cp ${setup.path}")
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

open class ExecutionTest(
    language: Language,
    setupFileName: String,
    setupFileExtension: String,
    runSetup: (File, File) -> String
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

                    // val btypes = File("java/src/main/kotlin/BTypes.java")

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

                            val resultString = runSetup(file, setupFile)
                            val resultMap = string2ResultMap(resultString)
                            for ((key, value) in execution.result) {
                                withClue("Expect $key = $value") {
                                    resultMap[key] shouldBe value
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
    val st = setup.getInstanceOf("setup") ?: throw EnvironmentException("Template 'setup' not found")
    st.add("name", mchName)
    st.add("execution", execution.operations)
    st.add("result", execution.result.keys)

    val directory = File("${outputDir.path}/$dir")
    if (!directory.exists()) directory.mkdir()
    val setupFile = File("${outputDir.path}/$dir/${mchName}Setup$setupFileExtension")
    setupFile.writeText(st.render())
    return setupFile
}

private fun string2ResultMap(string: String): Map<String, String> {
    return string.split("\n")
        .filter { it.isNotBlank() }
        .associate {
            val output = it.split("=")
            output[0].trim() to output[1].trim()
        }
}

data class Execution(val operations: List<String>, val result: Map<String, String>) {
    companion object {
        fun of(file: File): Execution {
            val content = file.readText()
            var operationsProcessed = false
            val operations = mutableListOf<String>()
            val result = mutableMapOf<String, String>()

            for (line in content.lines()) {
                // TODO: not hardcoded
                if (line.startsWith("***")) {
                    operationsProcessed = true
                    continue
                }

                if (operationsProcessed) {
                    val expect = line.split("=")
                    result[expect[0].trim()] = expect[1].trim()
                } else {
                    operations.add(line)
                }
            }

            return Execution(operations, result)
        }
    }
}