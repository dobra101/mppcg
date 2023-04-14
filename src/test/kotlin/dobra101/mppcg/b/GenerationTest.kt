package dobra101.mppcg.b

import dobra101.mppcg.Launcher
import dobra101.mppcg.Parser
import dobra101.mppcg.environment.Language
import io.kotest.assertions.throwables.shouldNotThrowAny
import io.kotest.assertions.withClue
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.longs.shouldBeExactly
import io.kotest.matchers.longs.shouldBeGreaterThan
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import java.io.File
import java.io.FileNotFoundException

class GenerationTest : ExpectSpec({
    fun File.toExpectations(): List<Expectation> {
        if (!exists()) throw FileNotFoundException(path)

        return readLines().map {
            val fields = it.split(",")
            if (fields.size != 5) {
                null
            } else {
                Expectation(
                    file = File(fields[0].trim()),
                    states = fields[1].trim().toLong(),
                    transitions = fields[2].trim().toLong(),
                    walltime = fields[3].trim().toLong(),
                    withDeadlock = fields[4].trim() == "true"
                )
            }
        }.filterNotNull().toList()
    }

    val expectations =
        File("src/test/resources/dobra101/mppcg/generate/operators/expectations.csv").toExpectations()
    val machines = File("src/test/resources/dobra101/mppcg/generate/operators/").walk()
        .filter { it.isFile && it.name.endsWith(".mch") }
        .toList()

    machines.filter { mch -> expectations.find { expect -> expect.file == mch } == null }
        .forEach { println("No expectation for: ${it.name}") }

    // TODO: check expected model checking results
    machines.forAll { machineFile ->
        context(machineFile.name) {
            println("Testing ${machineFile.name}")
            listOf(true, false).forAll { optimize ->
                val expectName = if (optimize) "optimized" else "regular"
                expect(expectName) {
                    val expectation = expectations.firstOrNull { it.file.name == machineFile.name }
                    withClue("No expectation given") {
                        expectation shouldNotBe null
                    }
                    var file: File? = null
                    shouldNotThrowAny {
                        file = Launcher.launch(
                            lang = Language.PROLOG,
                            file = machineFile,
                            parser = Parser.SableCC,
                            optimize = optimize,
                            benchmark = false
                        )
                    }

                    if (expectation!!.withDeadlock) {
                        // model check also without deadlock check
                        val result = Launcher.benchmarkProlog(file!!, checkDeadlock = false, timeout = expectation.walltime * 2)
                        withClue("Counterexample found") {
                            result.counterExample shouldBe null
                        }
                        withClue("States do not match") {
                            result.statesAnalysed shouldBeExactly expectation.states - 1 // because of start state
                        }
                        withClue("States do not match") {
                            result.transitionsFired shouldBeExactly expectation.transitions
                        }
                        // TODO: print hint if times differ too much
                    }

                    // run with deadlock check
                    val result = Launcher.benchmarkProlog(file!!, timeout = expectation.walltime * 2)
                    withClue("Counterexample found") {
                        if (expectation.withDeadlock) {
                            result.counterExample shouldNotBe null
                            result.counterExample!!.type shouldBe "deadlock"
                        } else {
                            result.counterExample shouldBe null
                        }
                    }
                    withClue("States do not match") {
                        result.statesAnalysed shouldBeExactly expectation.states - 1 // because of start state
                    }
                    withClue("States do not match") {
                        result.transitionsFired shouldBeExactly expectation.transitions
                    }
                    // TODO: print hint if times differ too much
                }
            }
        }
    }
})

private data class Expectation(
    val file: File,
    val states: Long,
    val transitions: Long,
    val walltime: Long,
    val withDeadlock: Boolean
)