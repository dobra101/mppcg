package dobra101.mppcg

import java.io.File

/**
 * Compiles the given files using files from the classpath.
 */
fun compile(cp: String, vararg files: File) {
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

/**
 * Executes the given file using files from the classpath.
 */
fun execute(cp: String, setup: File): String {
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