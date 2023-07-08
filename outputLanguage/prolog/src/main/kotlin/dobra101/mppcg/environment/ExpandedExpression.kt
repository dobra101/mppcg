package dobra101.mppcg.environment

import dobra101.mppcg.node.expression.Expression

/**
 * Utility class to render an expression node.
 *
 * Instead of rendering for example a resulting multiline string of output source code as a parameter of a
 * predicate call, only the variable name containing the result is rendered.
 * The multiline string is returned in the `before`-string, to generate syntactically correct Prolog code.
 */
data class ExpandedExpression(val before: String = "", val expression: String = "") {
    companion object {
        fun of(expression: Expression): ExpandedExpression {
            val expanded = expression.render()

            var before = if (expanded.rendered.isNotBlank() && expanded.containsKey(PrologOutputEnvironment.RESULT_EXPR)) {
                "${expanded.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}"
            } else {
                ""
            }

            val expr = if (expanded.containsKey(PrologOutputEnvironment.RESULT_EXPR)) {
                expanded[PrologOutputEnvironment.RESULT_EXPR].info
            } else {
                expanded.rendered
            }

            if (expanded.containsKey(PrologOutputEnvironment.BEFORE)) {
                before += expanded[PrologOutputEnvironment.BEFORE].info
            }

            return ExpandedExpression(before, expr)
        }
    }
}