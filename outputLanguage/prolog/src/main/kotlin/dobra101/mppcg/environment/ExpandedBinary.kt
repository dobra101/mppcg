package dobra101.mppcg.environment

import dobra101.mppcg.RenderResult
import dobra101.mppcg.node.MPPCGNode

/**
 * Utility class to expand binary expression.
 * Both child nodes of the expression node are rendered on after another.
 * Instead of rendering the entire source code string of each child node inside the template of the expression node,
 * this utility provides only the required information for the template.
 * The evaluations to execute before the expression node are stored in the `before`-string.
 */
data class ExpandedBinary(val before: String = "", val lhs: String = "", val rhs: String = "") {
    companion object {
        fun of(
            left: MPPCGNode, right: MPPCGNode
        ): ExpandedBinary {
            val lhsRendered = left.render()
            val rhsRendered = right.render()
            val before = StringBuilder()
            val lhs = createSideString(lhsRendered, before)
            val rhs = createSideString(rhsRendered, before)
            return ExpandedBinary(before.toString(), lhs, rhs)
        }

        fun of(
            left: MPPCGNode,
            right: MPPCGNode,
            differentBranches: Boolean,
            environment: PrologOutputEnvironment
        ): ExpandedBinary {
            val lhsRendered =
                environment.evaluatedExpressions.returnWithConditionedReset(differentBranches) { left.render() }
            val rhsRendered =
                environment.evaluatedExpressions.returnWithConditionedReset(differentBranches) { right.render() }

            val before = StringBuilder()
            val lhs = createSideString(lhsRendered, before)
            val rhs = createSideString(rhsRendered, before)
            return ExpandedBinary(before.toString(), lhs, rhs)
        }

        private fun createSideString(rendered: RenderResult, before: StringBuilder): String {
            val result = if (rendered.containsKey(PrologOutputEnvironment.RESULT_EXPR)) {
                if (rendered.rendered.isNotBlank()) {
                    before.append("${rendered.rendered}${PrologOutputEnvironment.EXPRESSION_SEPARATOR}")
                }
                rendered[PrologOutputEnvironment.RESULT_EXPR].info
            } else {
                rendered.rendered
            }
            if (rendered.containsKey(PrologOutputEnvironment.BEFORE)) {
                before.append(rendered[PrologOutputEnvironment.BEFORE].info)
            }
            return result
        }
    }
}