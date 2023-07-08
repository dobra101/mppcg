package dobra101.mppcg

import dobra101.mppcg.node.*
import dobra101.mppcg.node.b.*
import dobra101.mppcg.node.b.Function
import dobra101.mppcg.node.collection.*
import dobra101.mppcg.node.expression.*
import dobra101.mppcg.node.predicate.*
import dobra101.mppcg.node.substitution.*
import kotlin.math.min
import kotlin.reflect.*
import kotlin.reflect.full.*

object TypeInference {

    /**
     * Executes the HM type inference algorithm.
     *
     * @param node The programs root node
     */
    fun inferTypes(node: Program) {
        val env: MutableMap<MPPCGNode, Type> = mutableMapOf()
        analyse(node, env)
        env.filterKeys { it is ConcreteIdentifierExpression }
            .forEach {
                (it.key as ConcreteIdentifierExpression).type =
                    typeFromEnv((it.key as ConcreteIdentifierExpression).name.removePrefix("c_"), env)
            }

        // prune all type variables to get their concrete types
        pruneAll(node)
    }

    /**
     * Prune recursively all class variables of a given node via reflection.
     */
    private fun pruneAll(node: MPPCGNode) {
        val mppcgnode = typeOf<MPPCGNode?>()
        val collection = typeOf<Collection<MPPCGNode>>()
        val type = typeOf<Type?>()
        val string = typeOf<String?>()
        node::class.memberProperties
            .filter { !it.returnType.isSubtypeOf(string) }
            .forEach { kp ->
                if (kp.visibility == KVisibility.PRIVATE) return@forEach
                if (kp.call(node) == node || (node is ConcreteIdentifierExpression && kp.name == "node")) return@forEach

                if (kp.returnType.isSubtypeOf(mppcgnode)) {
                    kp.call(node)?.let { inst -> pruneAll(inst as MPPCGNode) }
                } else if (kp.returnType.isSubtypeOf(collection)) {
                    (kp.call(node) as Collection<*>).forEach { entry -> pruneAll(entry as MPPCGNode) }
                } else if (kp.returnType.isSubtypeOf(type)) {
                    (kp as? KMutableProperty1<out MPPCGNode, *>)?.setter?.call(node, prune(kp.call(node) as Type))
                }
            }
    }

    /**
     * The main HM algorithm.
     * Traverses the tree and calculates types for nodes.
     *
     * @param node The current visited note
     * @param env The context / environment which stores known types / type variables
     *
     * @return The calculated type(-variable) of the visited node
     */
    private fun analyse(node: MPPCGNode, env: MutableMap<MPPCGNode, Type>): Type {
        var nodeType: Type = TypeVariable()
        when (node) {
            // does not return a type but analyses
            is Predicate -> {
                analysePredicate(node, env)
                nodeType = MPPCG_Boolean
            }

            is Expression -> {
                nodeType = analyseExpression(node, env)
            }

            is Substitution -> {
                analyseSubstitution(node, env)
            }

            is Operation -> {
                val envBefore = HashMap(env)
                node.parameters.forEach {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }
                node.returnValues.forEach {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }
                node.body?.let { analyse(it, env) }

                if (node.returnValues.isEmpty()) return MPPCG_Void
                val type = if (node.returnValues.size == 1) {
                    node.returnValues[0].type!!
                } else {
                    TypeSet(node.returnValues[0].type!!)
                }
                node.type = type
                loadOldEnv(env, envBefore)
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
                    val type = TypeSet(elementType)
                    env[it] = type
                    it.type = type
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

            else -> {
                TODO("Not implemented for class: ${node.javaClass}")
            }
        }

        return nodeType
    }

    private fun analyseSubstitution(node: Substitution, env: MutableMap<MPPCGNode, Type>) {
        when (node) {
            is SequenceSubstitution -> {
                node.substitutions.forEach { analyse(it, env) }
            }

            is AssignSubstitution -> {
                val leftType = analyse(node.left, env)
                val rightType = analyse(node.right, env)
                unify(leftType, rightType)
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


            is ParallelSubstitution -> {
                node.substitutions.forEach { analyse(it, env) }
            }

            is WhileSubstitution -> {
                analyse(node.condition, env)
                analyse(node.body, env)
            }
        }
    }

    private fun analyseExpression(node: Expression, env: MutableMap<MPPCGNode, Type>): Type {
        when (node) {
            is IdentifierExpression -> {
                if (!env.contains(node)) {
                    env[node] = TypeVariable()
                }
                val type = typeFromEnv(node, env)
                if (node.type == null) node.type = type
                return type
            }

            is ValueExpression -> {
                return node.valueType
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

                val type = TypeRelation(fromType, exprType)
                loadOldEnv(env, envBefore)
                node.type = type
                return type
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
                unify(TypeSet(TypeRelation(leftSetType, rightSetType)), resultType)
                node.type = resultType
                return resultType
            }

            is UnaryFunctionExpression -> {
                val type = analyse(node.expression, env)
                val fromType = TypeVariable()
                val toType = TypeVariable()

                unify(TypeRelation(fromType, toType), type)
                val resultType = when (node.operator) {
                    UnaryFunctionOperator.DOMAIN -> TypeSet(fromType)
                    UnaryFunctionOperator.RANGE -> TypeSet(toType)
                    UnaryFunctionOperator.REVERSE -> TypeRelation(toType, fromType)
                }
                node.type = resultType
                return resultType
            }

            is Couple -> {
                val fromType = analyse(node.from, env)
                val toType = analyse(node.to, env)
                val type = TypeCouple(fromType, toType)
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
                        unify(TypeRelation(fromType, toType), leftType)
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
                unify(functionType, TypeRelation(fromType, toType))
                node.type = toType
                return toType
            }

            is ComprehensionSet -> {
                val envBefore = HashMap(env)
                val identifierTypes = node.identifiers.map {
                    env[it] = TypeVariable()
                    analyse(it, env)
                }

                analyse(node.predicates, env)
                val type = if (identifierTypes.size == 1) TypeSet(identifierTypes[0]) else TypeSet(
                    nestedCouples(identifierTypes)
                )
                loadOldEnv(env, envBefore)
                node.type = type
                return type
            }

            is EnumEntry -> {
                val type = TypeEnumValue(node.enum)
                env[node] = type
                node.type = type
                return type
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
                        TypeRelation(left, right)
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
                val types = node.elements.map { analyse(it, env) }.toList()
                for (i in 1 until types.size) {
                    unify(types[0], types[i])
                }
                val type = if (node.elements.isNotEmpty() && node.elements.first() is Couple) {
                    val couple = node.elements.first() as Couple
                    TypeRelation(couple.from.type!!, couple.to.type!!)
                } else {
                    TypeSet(
                        if (node.elements.isEmpty()) {
                            if (node.collectionType != null) {
                                node.collectionType!!
                            } else {
                                TypeVariable()
                            }
                        } else {
                            types[0]
                        }
                    )
                }

                if (node.collectionType != null) {
                    if (node.elements.isNotEmpty()) {
                        unify(node.collectionType!!, types[0])
                    }
                } else {
                    node.collectionType = type.type
                }

                node.type = type
                return type
            }


            is Sequence -> {
                node.elements.forEach { analyse(it, env) }
                val type = TypeOperator(
                    "sequence",
                    listOf((if (node.elements.isNotEmpty()) node.elements.first().type!! else TypeVariable()))
                )
                env[node] = type
                node.type = type
                return type
            }

            is BinarySequenceExpression -> {
                val leftType = analyse(node.left, env)
                val rightType = analyse(node.right, env)
                val type = when (node.operator) {
                    BinarySequenceExpressionOperator.APPEND -> leftType
                    BinarySequenceExpressionOperator.PREPEND -> rightType
                    BinarySequenceExpressionOperator.RESTRICT_TAIL -> leftType
                    BinarySequenceExpressionOperator.RESTRICT_FRONT -> leftType
                    else -> {
                        println("---------> $node")
                        TODO("BinarySequenceExpression Type")
                    }
                }
                node.type = type
                return type
            }

            is GeneralSumOrProductExpression -> {
                analyse(node.predicate, env)
                val type = analyse(node.expression, env)
                node.type = type
                return type
            }

            else -> {
                TODO("Not implemented for class: ${node.javaClass}")
            }
        }
    }

    private fun analysePredicate(node: Predicate, env: MutableMap<MPPCGNode, Type>): Type {
        when (node) {
            is BinaryLogicPredicate -> {
                analyse(node.left, env)
                analyse(node.right, env)
            }

            is UnaryLogicPredicate -> {
                analyse(node.predicate, env)
            }

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
                    if ((rightType as? TypeSet)?.type is TypeEnumValue) {
                        unify(leftType, TypeOperator(((rightType as TypeSet).type as TypeEnumValue).name, listOf()))
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
                loadOldEnv(env, envBefore)
            }

            is ValuePredicate -> {
                // nothing to analyse
            }

            // b
            is Invariant -> {
                node.predicates.forEach { analyse(it, env) }
            }

            else -> {
                TODO("Not implemented for class: ${node.javaClass}")
            }
        }

        return MPPCG_Boolean
    }

    private fun loadOldEnv(env: MutableMap<MPPCGNode, Type>, envBefore: MutableMap<MPPCGNode, Type>) {
        env.filterKeys { !envBefore.containsKey(it) }
            .forEach {
                val value = prune(it.value)
                when (it.key) {
                    is Expression -> {
                        (it.key as Expression).type = value
                    }

                    is Operation -> {
                        (it.key as Operation).type = value
                    }

                    else -> {
                        TODO("Not implemented")
                    }
                }
            }
        env.clear()
        env.putAll(envBefore)
    }

    private fun typeFromEnv(node: MPPCGNode, env: Map<MPPCGNode, Type>): Type {
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

    /**
     * Unifies two types, i.e. if one type is a type variable and the other is a concrete type,
     * the type variable's instance is set to the concrete type.
     *
     * @param type1 The first type
     * @param type2 The second type
     */
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
            if (((a.name != b.name) || (a.types.size != b.types.size))
                && !unifiable(a, b)
            ) {
                throw InferenceError("Types $a and $b do not match.")
            }
            for (i in 0 until min(a.types.size, b.types.size)) {
                unify(a.types[i], b.types[i])
            }
        }
    }

    private fun unifiable(type1: Type, type2: Type): Boolean {
        if (type1 is TypeNumber) {
            if (type1 == MPPCG_Real || type1 == MPPCG_Float) return type1 == type2
            return (type2 is TypeNumber && type2 != MPPCG_Real && type2 != MPPCG_Float)
        }
        return false
    }

    /**
     * Tries to retrieve the concrete type (the instance) of a type variable.
     *
     * @param type The type to prune
     * @return The pruned type
     */
    private fun prune(type: Type): Type {
        if (type is TypeVariable && type.instance != null) {
            type.instance = prune(type.instance!!)
            return type.instance!!
        }
        if (type is TypeRelation) {
            type.from = prune(type.from)
            type.to = prune(type.to)
        }
        if (type is TypeSet) {
            type.type = prune(type.type)
        }
        if (type is TypeCouple) {
            type.from = prune(type.from)
            type.to = prune(type.to)
        }
        if (type is TypeOperator && type.types.isNotEmpty()) {
            type.types = type.types.map { prune(it) }
            if (type !is TypeRelation && type is TypeSet && type.types.size == 1 && type.types[0] is TypeCouple) {
                val couple = type.types[0] as TypeCouple
                return TypeRelation(couple.from, couple.to)
            }
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

    private fun nestedCouples(types: List<Type>): Type {
        if (types.size == 1) return types[0]
        val couple = TypeCouple(types[0], types[1])
        if (types.size == 2) return couple

        val sublist = types.subList(2, types.size)
        return TypeCouple(couple, nestedCouples(sublist))
    }
}

class InferenceError(msg: String) : Exception(msg)