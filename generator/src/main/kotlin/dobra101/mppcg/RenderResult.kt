package dobra101.mppcg

import java.lang.Exception

/**
 * Wraps a source code string and individual information required/used by the environment.
 *
 * @property rendered The source code string
 * @property info A map containing individual information for the environment
 * @constructor Creates a Render result
 */
data class RenderResult(
    val rendered: String = "",
    val info: Map<String, IndividualInfo> = mapOf()
) {
    operator fun get(value: String): IndividualInfo {
        return info[value] ?: throw UnknownInfoException(value)
    }

    fun containsKey(key: String): Boolean {
        return info.containsKey(key)
    }
}

/**
 * Wrapper class containing information needed by an output environment.
 * The wrapping is used, as this class can be subclassed to provide further information types than strings.
 */
data class IndividualInfo(val info: String = "")

class UnknownInfoException(msg: String) : Exception(msg)