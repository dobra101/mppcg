package dobra101.mppcg

import de.be4.classicalb.core.parser.node.Start
import dobra101.mppcg.adapter.sablecc.convert
import dobra101.mppcg.environment.OutputLanguageEnvironment
import dobra101.mppcg.node.b.Machine
import java.io.File

class Generator {
    companion object {
        lateinit var environment: OutputLanguageEnvironment
    }

    fun generate(start: Start, outputPath: String): File {
        val machine: Machine = start.convert()

        val result = machine.render()

        val file = File("$outputPath${machine.name}.${environment.fileExtension}")
        file.createNewFile()
        file.writeText(result.rendered)
        return file
    }
}