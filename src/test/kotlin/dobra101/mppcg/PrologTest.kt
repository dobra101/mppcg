package dobra101.mppcg

import dobra101.mppcg.environment.Language
import dobra101.mppcg.prob.ProBResult
import dobra101.mppcg.prob.ProBResultAnalyser
import io.kotest.assertions.withClue
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.longs.shouldBeExactly
import io.kotest.matchers.shouldBe
import java.io.File

class PrologTest : ExpectSpec({
    val include = listOf("TrafficLight.mch", "Lift.mch", "scheduler_deterministic_MC.mch")

    val machines = File("src/main/resources/machines/").walk()
        .filter { it.isFile && it.name.endsWith(".mch") }
        .filter { if (include.isNotEmpty()) include.contains(it.name) else true }
        .toList()

    fun runProB(machine: File): ProBResult {
        val prologResourcesPath = "prolog/src/main/resources"
        val probPath = "$prologResourcesPath/ProB_Signed/probcli.sh"
        val probArgs =
            "--model-check -disable-time-out -p OPERATION_REUSE full -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory"

        val cmd = "$probPath $probArgs ${machine.path}"
        val process: Process = Runtime.getRuntime().exec(cmd)
        process.waitFor()

        val probOutput = process.inputReader().readText()
        val probOutputFile = File(machine.path.replace(".mch", ".prob"))
        if (probOutputFile.exists()) probOutputFile.delete()
        return ProBResultAnalyser.analyze(probOutput)
    }

    machines.forAll { machineFile ->
        context(machineFile.name) {
            println("Testing ${machineFile.name}")
            val mchResult = runProB(machineFile)
            println("ProB Result: $mchResult")
            listOf(true, false).forAll { optimize ->
                val expectName = if (optimize) "optimized" else "regular"
                expect(expectName) {
                    val file = Launcher.launch(
                        lang = Language.PROLOG,
                        file = machineFile.name,
                        parser = Parser.SableCC,
                        optimize = optimize,
                        benchmark = false
                    )
                    val result = Launcher.benchmarkProlog(file)

                    withClue("States do not match") {
                        result.statesAnalysed shouldBeExactly mchResult.statesAnalysed
                    }
                    withClue("Transitions do not match") {
                        result.transitionsFired shouldBeExactly mchResult.transitionsFired
                    }
                    result.counterExampleFound shouldBe mchResult.counterExampleFound
                }
            }
        }
    }
})