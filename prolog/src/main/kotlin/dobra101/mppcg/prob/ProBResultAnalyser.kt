package dobra101.mppcg.prob

object ProBResultAnalyser {
    private val timeRegex = "^Model checking time: (\\d*) ms \\((\\d*) ms walltime\\)".toRegex()
    private val statesRegex = "^States analysed: (\\d*)".toRegex()
    private val transitionsRegex = "^Transitions fired: (\\d*)".toRegex()
    private val memoryRegex = "^ProB memory used:\\s*(\\d*\\.\\d*) MB \\( (\\d*\\.\\d*) MB program\\)".toRegex()

    // TODO: memoryRegex not working
    fun analyze(probOutput: String): ProBResult {
        val builder = ProBResult.Builder()
        val lines = probOutput.split("\n")
        lines.forEach {
            timeRegex.matchEntire(it)?.let { res ->
                builder.modelCheckingTime(res.groupValues[1].toLong()).wallTime(res.groupValues[2].toLong())
            } ?: statesRegex.matchEntire(it)?.let { res -> builder.statesAnalysed(res.groupValues[1].toLong()) }
            ?: transitionsRegex.matchEntire(it)?.let { res -> builder.transitionsFired(res.groupValues[1].toLong()) }
            ?: memoryRegex.matchEntire(it)?.let { res ->
                builder.memoryUsedTotal(res.groupValues[1].toDouble()).memoryUsedProgram(res.groupValues[2].toDouble())
            }
        }

        return builder.build()
    }
}

data class ProBResult(
    val modelCheckingTime: Long,
    val wallTime: Long,
    val statesAnalysed: Long,
    val transitionsFired: Long,
    val counterExampleFound: Boolean,
    val memoryUsedTotal: Double,
    val memoryUsedProgram: Double
) {
    data class Builder(
        private var modelCheckingTime: Long? = null,
        private var wallTime: Long? = null,
        private var statesAnalysed: Long? = null,
        private var transitionsFired: Long? = null,
        private var counterExampleFound: Boolean? = null,
        private var memoryUsedTotal: Double? = null,
        private var memoryUsedProgram: Double? = null
    ) {
        fun modelCheckingTime(modelCheckingTime: Long) = apply { this.modelCheckingTime = modelCheckingTime }
        fun wallTime(wallTime: Long) = apply { this.wallTime = wallTime }
        fun statesAnalysed(statesAnalysed: Long) = apply { this.statesAnalysed = statesAnalysed }
        fun transitionsFired(transitionsFired: Long) = apply { this.transitionsFired = transitionsFired }
        fun counterExampleFound(counterExampleFound: Boolean) = apply { this.counterExampleFound = counterExampleFound }
        fun memoryUsedTotal(memoryUsedTotal: Double) = apply { this.memoryUsedTotal = memoryUsedTotal }
        fun memoryUsedProgram(memoryUsedProgram: Double) = apply { this.memoryUsedProgram = memoryUsedProgram }
        fun build() = ProBResult(
            modelCheckingTime ?: -1,
            wallTime ?: -1,
            statesAnalysed ?: -1,
            transitionsFired ?: -1,
            counterExampleFound ?: false, // TODO: with regex
            memoryUsedTotal ?: -1.0,
            memoryUsedProgram ?: -1.0
        )
    }
}