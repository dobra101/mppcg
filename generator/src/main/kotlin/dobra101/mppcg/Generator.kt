package dobra101.mppcg

import de.be4.classicalb.core.parser.node.Start
import dobra101.mppcg.environment.OutputLanguageEnvironment

class Generator {
    companion object {
        lateinit var environment: OutputLanguageEnvironment
    }

    fun generate(start: Start) {
        // TODO: convert
//        val machine: Machine =

//        val result = machine.render()

//        val file = File("generator/build/generated/${machine.name}.${environment.fileExtension}")
//        file.createNewFile()
//        file.writeText(result.rendered)
    }
}