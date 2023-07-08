package dobra101.mppcg.environment

import dobra101.mppcg.node.collection.SetEntry
import dobra101.mppcg.node.expression.Expression
import dobra101.mppcg.node.expression.ValueExpression

/**
 * Utility class to expand a list of expressions.
 *
 * Use cases are for example predicate calls which have a list of parameters.
 * These parameters have to be rendered in the correct order, one after another.
 */
data class ExpandedExpressionList(val before: String = "", val expressions: List<String> = emptyList()) {
    companion object {
        fun of(expressions: List<Expression>): ExpandedExpressionList {
            var before = ""
            val expression = expressions.map {
                val result = it.render()

                if (it is ValueExpression || it is SetEntry) {
                    result.rendered
                } else {
                    if (result.rendered.isNotBlank() && result.containsKey(PrologOutputEnvironment.RESULT_EXPR)) {
                        before += "${result.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
                    }
                    if (result.containsKey(PrologOutputEnvironment.BEFORE)) {
                        before += result[PrologOutputEnvironment.BEFORE].info
                    }
                    if (result.info.containsKey(PrologOutputEnvironment.RESULT_EXPR)) result[PrologOutputEnvironment.RESULT_EXPR].info
                    else result.rendered
                }
            }
            return ExpandedExpressionList(before, expression)
        }
    }
}