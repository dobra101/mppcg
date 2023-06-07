package dobra101.mppcg.b

import dobra101.mppcg.Launcher
import dobra101.mppcg.Parser
import dobra101.mppcg.environment.Language
import io.kotest.assertions.throwables.shouldNotThrowAny
import io.kotest.assertions.withClue
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.longs.shouldBeExactly
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import java.io.File
import java.io.FileNotFoundException

class GenerationTest : ExpectSpec({
    fun File.toExpectations(): List<Expectation> {
        if (!exists()) throw FileNotFoundException(path)

        return readLines().mapNotNull {
            val fields = it.split(",")
            if (fields.size < 6) {
                null
            } else {
                Expectation(
                    file = File(fields[0].trim()),
                    states = fields[1].trim().toLong(),
                    transitions = fields[2].trim().toLong(),
                    walltime = fields[3].trim().toLong(),
                    withDeadlock = fields[4].trim() == "true",
                    withInvariantViolation = fields[5].trim() == "true",
                    minInt = if (fields.size == 8) fields[6].trim().toLong() else -10,
                    maxInt = if (fields.size == 8) fields[7].trim().toLong() else 10,
                )
            }
        }.toList()
    }

    val expectations =
        File("src/test/resources/dobra101/mppcg/generate/operators/expectations.csv").toExpectations()
    val machines = File("src/test/resources/dobra101/mppcg/generate/operators/").walk()
        .filter { it.isFile && it.name.endsWith(".mch") }
        .toList()

    machines.filter { mch -> expectations.find { expect -> expect.file == mch } == null }
        .forEach { println("No expectation for: ${it.name}") }

    // TODO: check expected model checking results
    machines
        .sortedBy { expectations.find { ex -> ex.file == it }?.walltime }
        .forAll { machineFile ->
            context(machineFile.name) {
                println("Testing ${machineFile.name}")
                listOf(true).forAll { optimize ->
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
                                benchmark = false,
                                minInt = expectation!!.minInt,
                                maxInt = expectation.maxInt
                            )
                        }

                        println("Running ProB for at most ${(1000 + expectation!!.walltime * 2) / 1000} seconds")

                        if (expectation.withDeadlock) {
                            println("NOT CHECKING DEADLOCK")
                            // model check also without deadlock check
                            val result = Launcher.benchmarkProlog(
                                file!!,
                                checkDeadlock = false,
                                checkInvariant = true,
                                timeout = 1000 + expectation.walltime * 2
                            )
                            withClue("Counterexample found") {
                                if (!expectation.withInvariantViolation) {
                                    result.counterExample shouldBe null
                                } else {
                                    result.counterExample shouldNotBe null
                                }
                            }
                        }

                        if (expectation.withInvariantViolation) {
                            println("NOT CHECKING INVARIANT")
                            // model check also without invariant check
                            val result = Launcher.benchmarkProlog(
                                file!!,
                                checkDeadlock = true,
                                checkInvariant = false,
                                timeout = 1000 + expectation.walltime * 2
                            )
                            withClue("Counterexample found") {
                                if (!expectation.withDeadlock) {
                                    result.counterExample shouldBe null
                                } else {
                                    result.counterExample shouldNotBe null
                                }
                            }
                        }


                        val result = Launcher.benchmarkProlog(
                            file!!,
                            checkDeadlock = !expectation.withDeadlock,
                            checkInvariant = !expectation.withInvariantViolation,
                            timeout = 1000 + expectation.walltime * 2
                        )
                        withClue("Counterexample found") {
                            result.counterExample shouldBe null
                        }
                        withClue("States do not match") {
                            result.statesAnalysed shouldBeExactly expectation.states
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
    val withDeadlock: Boolean,
    val withInvariantViolation: Boolean,
    val minInt: Long,
    val maxInt: Long
)