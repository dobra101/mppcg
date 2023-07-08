package dobra101.mppcg

import dobra101.mppcg.b.outputDir
import dobra101.mppcg.environment.Language
import dobra101.mppcg.prob.ProBResult
import dobra101.mppcg.prob.ProBResultAnalyser
import io.kotest.assertions.throwables.shouldNotThrowAny
import io.kotest.core.spec.style.ExpectSpec
import io.kotest.inspectors.forAll
import java.io.File

class JavaTest : ExpectSpec({
    fun readExpectedResults(file: File, machines: List<File>): Map<File, ProBResult> {
        val result: MutableMap<File, ProBResult> = mutableMapOf()
        val content = file.readLines()
        for (line: String in content) {
            val split = line.split(",")
            val mchName = split[0]
            val states = split[1].toLong()
            val transitions = split[2].toLong()
            val time = split[3].toLong()

            val mchFile = machines.find { it.name == mchName } ?: continue
            result[mchFile] = ProBResult.Builder()
                .modelCheckingTime(time)
                .statesAnalysed(states)
                .transitionsFired(transitions)
                .build()
        }
        return result
    }

    val include: List<String> = listOf(
        "Lift.mch",
        "TrafficLight_MC.mch",
        "scheduler_deterministic_MC.mch",
        "QueensWithEvents_4.mch",
        "QueensWithEvents_8.mch",
        "sort_m2_data1000.mch",
        "CAN_BUS_tlc.mch",
        "Cruise_finite1_deterministic_MC.mch",
        "LandingGear_R6.mch",
        "Train_1_beebook_deterministic_MC.mch"
    )

    val exclude: List<String> = listOf()
    val machines = File("src/main/resources/machines/").walk()
        .filter { it.isFile && it.name.endsWith(".mch") }
        .filter { if (include.isNotEmpty()) include.contains(it.name) else true }
        .toList()

    val expectedModelCheckingResults: Map<File, ProBResult> =
        readExpectedResults(File("src/test/resources/dobra101/mppcg/expectedModelcheckingResults.csv"), machines)

    fun runProB(machine: File): ProBResult {
        val prologResourcesPath = "outputLanguage/prolog/src/main/resources"
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
            var mchResult = expectedModelCheckingResults[machineFile]
            println("ProB Result: $mchResult")
            if (mchResult == null) {
                println("No result specified. Running ProB...")
                mchResult = runProB(machineFile)
                println(mchResult)
            }
            val file = Launcher.launch(
                lang = Language.JAVA,
                file = machineFile.name,
                parser = Parser.SableCC,
                benchmark = false
            )
            shouldNotThrowAny {
                compile(cp = outputDir.path + "/${machineFile.path}:inputLanguage/B/java/build/libs/btypes.jar", file)
            }
        }
    }
})