package dobra101.mppcg

import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import kotlin.math.min

class TypeInference {

    fun infereTypes(node: Program) {
        val env: MutableMap<MPPCGNode, Type> = mutableMapOf()
        analyse(node, env)
        env.forEach {
            if (it.key is Expression) {
                if ((it.key as Expression).type == null) {
                    (it.key as Expression).type = it.value
                }
                (it.key as Expression).type =
                    prune((it.key as Expression).type!!) // TODO: exception if type variable is kept
            }
        }
    }

    // ast muss gewisse nodes mit typen haben

    // env: Jeder Wertvariablen wird ein Typ zugeordnet
    private fun analyse(node: MPPCGNode, env: MutableMap<MPPCGNode, Type>): Type {
        // TODO: env setup in B
        when (node) {
            // does not return a type but analyses
            is Predicate -> {
                analysePredicate(node, env)
                return MPPCG_Boolean
            }

            is IdentifierExpression -> {
                val type = typeFromEnv(node, env, true)
                if (node.type == null) node.type = prune(type)
                return prune(type)
            }

            is ValueExpression -> {
                return node.type!!
            }

            is BinaryExpression -> {
                val leftType = prune(analyse(node.left, env))
                val rightType = prune(analyse(node.right, env))
                val nonEnumValueRight =
                    if (rightType is TypeEnumValue) TypeOperator(rightType.name, listOf()) else rightType

                val resultType =
                    if (node.operator == BinaryExpressionOperator.MULT && (leftType is TypeSet || nonEnumValueRight is TypeSet)) {
                        val left = (leftType as? TypeSet)?.type ?: leftType
                        val right = (nonEnumValueRight as? TypeSet)?.type ?: nonEnumValueRight
                        TypeSet(TypeOperator("couple", listOf(left, right)))
                    } else {
                        unify(leftType, nonEnumValueRight)
                        leftType
                    }

                node.type = resultType
                return resultType
            }

            is UnaryCollectionExpression -> {
                val collectionType = analyse(node.collection, env)
                val type = when (node.operator) {
                    UnaryCollectionOperator.CARD,
                    UnaryCollectionOperator.MAX,
                    UnaryCollectionOperator.MIN -> {
                        MPPCG_Integer
                    }

                    UnaryCollectionOperator.POW,
                    UnaryCollectionOperator.POW1 -> {
                        TypeSet(collectionType)
                    }
                }
                node.type = type
                return type
            }

            is EnumCollectionNode -> {
                val type = typeFromEnv(node, env)
                node.type = type
                return type
            }

            is BinaryCollectionExpression -> {
                val leftType = analyse(node.left, env)
                val rightType = analyse(node.right, env)
                val type = when (node.operator) {
                    BinaryCollectionOperator.PRJ1 -> leftType
                    BinaryCollectionOperator.PRJ2 -> rightType
                    BinaryCollectionOperator.INTERSECTION,
                    BinaryCollectionOperator.SUBTRACTION,
                    BinaryCollectionOperator.UNION,
                    BinaryCollectionOperator.CONCAT -> {
                        unify(leftType, rightType)
                        leftType
                    }
                }
                node.type = type
                return type
            }

            is AnonymousCollectionNode -> {
                if (node.elements.isEmpty()) return TypeVariable()

                val types = node.elements.map { analyse(it, env) }.toList()
                for (i in 1 until types.size) {
                    unify(types[0], types[i])
                }
                val type = TypeSet(types[0])
                node.type = type
                return type
            }

            is SequenceSubstitution -> {
                node.substitutions.forEach { analyse(it, env) }
            }

            is AssignSubstitution -> {
                val leftType = analyse(node.left, env)
                val rightType = analyse(node.right, env)
                unify(leftType, rightType)
            }

            is Operation -> {
                val envBefore = HashMap(env)
                node.parameters.forEach {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }
                node.returnValues.forEach { analyse(it, env) }
                node.body?.let { analyse(it, env) }

                if (node.returnValues.isEmpty()) return MPPCG_Void
                if (node.returnValues.size == 1) return node.returnValues[0].type!!
                val type = TypeSet(node.returnValues[0].type!!)
                node.type = type
                env.clear()
                env.putAll(envBefore)
                env[node] = node.type
                return type
            }

            is Machine -> {
                // add all nodes which need a type
                node.constants.forEach { env[it] = TypeVariable() }
                node.concreteConstants.forEach { env[it] = TypeVariable() }
                node.variables.variables.forEach { env[it] = TypeVariable() }
                node.concreteVariables.forEach { env[it] = TypeVariable() }
                node.sets.forEach {
                    val elementType = TypeOperator(it.name, listOf())
                    env[it] = TypeSet(elementType)
                    it.elements.forEach { e -> analyse(e, env) }
                }

                node.properties.forEach { analyse(it, env) }
                node.invariant.predicates.forEach { analyse(it, env) }
                node.parameters.forEach { analyse(it, env) }
                node.constraints?.let { analyse(it, env) }
                node.definitions?.let { analyse(it, env) }
                node.initialization.substitutions.forEach { analyse(it, env) }
                node.assertions.forEach { analyse(it, env) }
                node.operations.forEach { analyse(it, env) }
            }

            is Select -> {
                analyse(node.condition, env)
                node.then?.let { analyse(it, env) }
                node.whenSubstitution.forEach { analyse(it, env) }
                node.elseSubstitution?.let { analyse(it, env) }
            }

            is IfSubstitution -> {
                analyse(node.condition, env)
                analyse(node.then, env)
                node.elseIf.forEach { analyse(it, env) }
                node.elseSubstitution?.let { analyse(it, env) }
            }

            is ElseIfSubstitution -> {
                analyse(node.condition, env)
                analyse(node.then, env)
            }

            is Precondition -> {
                node.substitution?.let { analyse(it, env) }
                analyse(node.predicate, env)
            }

            is IntervalExpression -> {
                val leftType = analyse(node.left, env)
                val rightType = analyse(node.right, env)
                unify(leftType, rightType)
                node.type = TypeSet(leftType)

                return node.type!!
            }

            is InfiniteSet -> {
                val type = TypeVariable()
                unify(TypeSet(node.setType), type)
                val pruned = prune(type)
                node.type = pruned
                return pruned
            }

            is Function -> {
                val leftType = analyse(node.left, env)
                val rightType = analyse(node.right, env)

                val leftSetType = TypeVariable()
                val rightSetType = TypeVariable()
                unify(TypeSet(leftSetType), leftType)
                unify(TypeSet(rightSetType), rightType)
                val resultType = TypeVariable()
                unify(TypeRelation(leftSetType, rightSetType), resultType)
                node.type = resultType
                return resultType
            }

            is UnaryFunctionExpression -> {
                val type = analyse(node.expression, env)
                val fromType = TypeVariable()
                val toType = TypeVariable()

                unify(TypeSet(TypeOperator("couple", listOf(fromType, toType))), type)
                val resultType = when (node.operator) {
                    UnaryFunctionOperator.DOMAIN -> TypeSet(fromType)
                    UnaryFunctionOperator.RANGE -> TypeSet(toType)
                    UnaryFunctionOperator.REVERSE -> TypeSet(TypeOperator("couple", listOf(toType, fromType)))
                }
                node.type = resultType
                return resultType
            }

            is ParallelSubstitution -> {
                node.substitutions.forEach { analyse(it, env) }
            }

            is Couple -> {
                val fromType = analyse(node.from, env)
                val toType = analyse(node.to, env)
                val type = TypeOperator("couple", listOf(fromType, toType))
                node.type = type
                return type
            }

            is UnaryExpression -> {
                val nodeType = analyse(node.value, env)
                val type = when (node.operator) {
                    UnaryExpressionOperator.CONVERT_BOOLEAN -> MPPCG_Boolean
                    UnaryExpressionOperator.SUCC -> nodeType
                    UnaryExpressionOperator.PRED -> nodeType
                    UnaryExpressionOperator.MINUS -> {
                        when (nodeType) {
                            MPPCG_Nat1, MPPCG_Nat -> MPPCG_Int
                            MPPCG_Natural, MPPCG_Natural1 -> MPPCG_Integer
                            else -> nodeType
                        }
                    }
                }
                node.type = type
                return type
            }

            is BinaryFunctionExpression -> {
                val leftType = prune(analyse(node.left, env))
                val rightType = prune(analyse(node.right, env))

                val type = when (node.operator) {
                    BinaryFunctionOperator.DOMAIN_RESTRICTION,
                    BinaryFunctionOperator.DOMAIN_SUBTRACTION -> rightType

                    BinaryFunctionOperator.IMAGE -> {
                        val callType = (rightType as? TypeSet)?.type ?: rightType

                        val fromType = TypeVariable()
                        val toType = TypeVariable()
                        if (leftType is TypeRelation) {
                            unify(TypeRelation(fromType, toType), leftType)
                        } else {
                            unify(TypeSet(TypeOperator("couple", listOf(fromType, toType))), leftType)
                        }
                        unify(fromType, callType)
                        TypeSet(toType)
                    }

                    BinaryFunctionOperator.OVERWRITE,
                    BinaryFunctionOperator.RANGE_RESTRICTION,
                    BinaryFunctionOperator.RANGE_SUBTRACTION,
                    BinaryFunctionOperator.ITERATE -> leftType

                    BinaryFunctionOperator.FORWARD_COMPOSITION -> {
                        val fromType = (leftType as TypeRelation).from
                        val toType = (rightType as TypeRelation).to
                        val yType = TypeVariable()
                        unify(yType, leftType.to)
                        unify(yType, rightType.from)
                        TypeRelation(fromType, toType)
                    }
                }
                node.type = type
                return type
            }

            is CallFunctionExpression -> {
                val functionType = analyse(node.expression, env)
                val parameterTypes = node.parameters.map { analyse(it, env) }
                val fromType = TypeVariable()
                if (parameterTypes.isNotEmpty()) {
                    unify(fromType, parameterTypes[0])
                    for (i in 1 until parameterTypes.size) {
                        unify(fromType, parameterTypes[i])
                    }
                }
                val toType = TypeVariable()
                unify(functionType, TypeSet(TypeOperator("couple", listOf(fromType, toType))))
                return toType
            }

            is LambdaExpression -> {
                val envBefore = HashMap(env)
                val identifierTypes = node.identifiers.map {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }
                analyse(node.predicate, env)
                val exprType = analyse(node.expression, env)
                val fromType =
                    if (identifierTypes.size == 1) identifierTypes[0] else TypeOperator("list", identifierTypes)

                val type = TypeSet(TypeOperator("couple", listOf(fromType, exprType)))
                env.clear()
                env.putAll(envBefore)
                node.type = type
                return type
            }

            is ComprehensionSet -> {
                val envBefore = HashMap(env)
                val identifierTypes = node.identifiers.map {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }

                analyse(node.predicates, env)
                // TODO: nested couples for more than two identifiers?
                val type = if (identifierTypes.size == 1) TypeSet(identifierTypes[0]) else TypeSet(
                    TypeOperator(
                        "couple",
                        identifierTypes
                    )
                )
                env.clear()
                env.putAll(envBefore)
                node.type = type
                return type
            }

            is EnumEntry -> {
                val type = TypeEnumValue(node.enum)
                env[node] = type
                return type
            }

            else -> {
                TODO("Not implemented for class: ${node.javaClass}")
            }
        }
        return TypeVariable()
    }

    private fun analysePredicate(node: Predicate, env: MutableMap<MPPCGNode, Type>): Type {
        when (node) {
            // common
            is BinaryLogicPredicate -> {
                analyse(node.left, env)
                analyse(node.right, env)
            }

            is UnaryLogicPredicate -> {
                analyse(node.predicate, env)
            }

            // TODO: refactor enum handling
            // TODO: member is only in B
            is BinaryPredicate -> {
                val leftType = analyse(node.left, env)
                val rightType = prune(analyse(node.right, env))
                if (node.operator != BinaryPredicateOperator.MEMBER && node.operator != BinaryPredicateOperator.NOT_MEMBER) {
                    if (rightType is TypeEnumValue) {
                        unify(leftType, TypeOperator(rightType.name, listOf()))
                    } else {
                        unify(leftType, rightType)
                    }
                } else {
                    if ((rightType as TypeSet).type is TypeEnumValue) {
                        unify(leftType, TypeOperator((rightType.type as TypeEnumValue).name, listOf()))
                    } else {
                        unify(TypeSet(leftType), rightType)
                    }
                }
            }

            is QuantifierPredicate -> {
                val envBefore = HashMap(env)
                node.identifier.forEach {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }
                analyse(node.predicate, env)
                node.quantification?.let { analyse(it, env) }
                env.clear()
                env.putAll(envBefore)
            }

            is ValuePredicate -> {
                // nothing to analyse
            }

            // b
            is Invariant -> {
                node.predicates.forEach { analyse(it, env) }
            }
        }

        return MPPCG_Boolean
    }

    private fun typeFromEnv(node: MPPCGNode, env: Map<MPPCGNode, Type>, acceptTypeEnum: Boolean = false): Type {
        val type = env[node]
        if (type != null) return type
        if (node is IdentifierExpression) {
            for ((n, t) in env) {
                if (n is EnumEntry && n.name == node.name) {
                    return t
                } else if (n is ConcreteIdentifierExpression && n.name == "c_${node.name}") {
                    return t
                }
            }
        }

        throw InferenceError("Undefined symbol $node")
    }

    private fun typeFromEnv(name: String, env: Map<MPPCGNode, Type>): Type {
        val key = env.keys.first {
            (it as? IdentifierExpression)?.name == name
                    || (it as? ConcreteIdentifierExpression)?.name == name
                    || (it as? EnumEntry)?.name == name
        }
        return typeFromEnv(key, env)
    }

    private fun unify(type1: Type, type2: Type) {
        val a = prune(type1)
        val b = prune(type2)
        if (a is TypeVariable) {
            if (a != b) {
                if (occursInType(a, b)) {
                    occursInType(a, b)
                    throw InferenceError("Recursive unification of $a and $b")
                }
                a.instance = b
            }
        } else if (a is TypeOperator && b is TypeVariable) {
            unify(b, a)
        } else if (a is TypeOperator && b is TypeOperator) {


            if (((a.name != b.name) || (a.types.size != b.types.size)) && !unifyable(
                    a,
                    b
                )
            ) { // TODO: and to or, min types size not necessary in for loop
                throw InferenceError("Types $a and $b do not match.")
            }
            for (i in 0 until min(a.types.size, b.types.size)) {
                unify(a.types[i], b.types[i])
            }
        }
    }

    private fun unifyable(type1: Type, type2: Type): Boolean {
        if (type1 is TypeNumber) {
            if (type1 == MPPCG_Real || type1 == MPPCG_Float) return type1 == type2
            return (type2 is TypeNumber && type2 != MPPCG_Real && type2 != MPPCG_Float)
        }
        return false
    }

    private fun prune(type: Type): Type {
        if (type is TypeVariable && type.instance != null) {
            type.instance = prune(type.instance!!)
            return type.instance!!
        }
        return type
    }

    private fun occursInType(typeVariable: TypeVariable, other: Type): Boolean {
        val pruned = prune(other)
        if (pruned == typeVariable) {
            return true
        } else if (pruned is TypeOperator) return pruned.types.find { occursInType(typeVariable, it) } != null
        return false
    }
}

class InferenceError(msg: String) : Exception(msg)