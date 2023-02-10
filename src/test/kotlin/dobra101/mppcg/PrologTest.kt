package dobra101.mppcg

import dobra101.mppcg.environment.Language
import dobra101.mppcg.prob.ProBResult
import dobra101.mppcg.prob.ProBResultAnalyser
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.longs.shouldBeExactly
import io.kotest.matchers.shouldBe
import java.io.File

class PrologTest : ExpectSpec({
    fun runProB(machineName: String): ProBResult {
        val prologResourcesPath = "prolog/src/main/resources"
        val probPath = "$prologResourcesPath/ProB_Signed/probcli.sh"
        val probArgs =
            "--model-check -disable-time-out -p OPERATION_REUSE full -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory"
        val mchFilePath = "src/main/resources/machines/$machineName.mch"

        val cmd = "$probPath $probArgs $mchFilePath"
        val process: Process = Runtime.getRuntime().exec(cmd)
        process.waitFor()

        val probOutput = process.inputReader().readText()
        val probOutputFile = File(mchFilePath.replace(".mch", ".prob"))
        if (probOutputFile.exists()) probOutputFile.delete()
        return ProBResultAnalyser.analyze(probOutput)
    }

    listOf("Lift_PG", "TrafficLight").forAll { machineName ->
        context(machineName) {
            val mchResult = runProB(machineName)
            listOf(true, false).forAll { optimize ->
                val expectName = if (optimize) "optimized" else "regular"
                expect(expectName) {
                    val file = Launcher.launch(
                        lang = Language.PROLOG,
                        file = machineName,
                        parser = Parser.SableCC,
                        optimize = optimize,
                        benchmark = false
                    )
                    val result = Launcher.benchmarkProlog(file)

                    // add one because of XTL
                    result.statesAnalysed shouldBeExactly mchResult.statesAnalysed + 1
                    result.transitionsFired shouldBeExactly mchResult.transitionsFired + 1
                    result.counterExampleFound shouldBe mchResult.counterExampleFound
                }
            }
        }
    }
})