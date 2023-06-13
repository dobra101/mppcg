package dobra101.mppcg.node

import java.util.Objects

interface Type

// represents an arbitrary type
data class TypeVariable(var instance: Type? = null) : Type {
    val count = typeVariableCount++

    companion object {
        private var typeVariableCount = 0
    }

    override fun toString(): String {
        if (instance == null) return "TypeVariable(count=$count)"
        return "TypeVariable(instance=$instance)"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is TypeVariable) return false
        return other.count == count && other.instance == instance
    }

    override fun hashCode(): Int {
        return Objects.hash(count, instance)
    }
}

open class TypeOperator(open val name: String, open var types: List<Type>) : Type {
    override fun toString(): String {
        return "TypeOperator(name=$name, types=$types)"
    }
}

data class TypeRelation(var from: Type, var to: Type) : TypeSet(TypeCouple(from, to))

open class TypeSet(var type: Type) : TypeOperator(name = "set", types = listOf(type))

data class TypeCouple(var from: Type, var to: Type) : TypeOperator(name = "couple", types = listOf(from, to))

// TODO: needed?
open class BaseType(override val name: String, override var types: List<Type>) : TypeOperator(name, types)

data class TypeNumber(override val name: String) : BaseType(name, listOf())

// TODO: some are only for B
val MPPCG_Real = TypeNumber("real") // HINT: B
val MPPCG_Float = TypeNumber("float")
val MPPCG_Nat = TypeNumber("nat0") // HINT: B
val MPPCG_Nat1 = TypeNumber("nat1") // HINT: B
val MPPCG_Integer = TypeNumber("integer") // HINT: B
val MPPCG_Natural = TypeNumber("natural") // HINT: B
val MPPCG_Natural1 = TypeNumber("natural1") // HINT: B
val MPPCG_Int = TypeNumber("int")
val MPPCG_Boolean = BaseType("boolean", listOf())
val MPPCG_String = BaseType("string", listOf())
val MPPCG_Void = BaseType("void", listOf())

data class TypeEnumValue(override val name: String) : TypeOperator(name, listOf())

class TypeException(msg: String) : Exception("Unknown type $msg")