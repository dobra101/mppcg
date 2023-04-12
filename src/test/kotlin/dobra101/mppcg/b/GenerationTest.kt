package dobra101.mppcg.b

import dobra101.mppcg.Launcher
import dobra101.mppcg.Parser
import dobra101.mppcg.environment.Language
import io.kotest.assertions.throwables.shouldNotThrowAny
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import java.io.File

class GenerationTest : ExpectSpec({
    val machines = File("src/test/resources/dobra101/mppcg/generate/operators/").walk()
        .filter { it.isFile && it.name.endsWith(".mch") }
        .toList()

    machines.forAll { machineFile ->
        context(machineFile.name) {
            println("Testing ${machineFile.name}")
            listOf(true, false).forAll { optimize ->
                val expectName = if (optimize) "optimized" else "regular"
                expect(expectName) {
                    shouldNotThrowAny {
                        val file = Launcher.launch(
                            lang = Language.PROLOG,
                            file = machineFile,
                            parser = Parser.SableCC,
                            optimize = optimize,
                            benchmark = false
                        )

                        Launcher.benchmarkProlog(file)
                    }
                }
            }
        }
    }
})