package dobra101.mppcg.node

import dobra101.mppcg.node.b.FunctionType
import dobra101.mppcg.node.collection.CollectionType
import java.util.Objects

interface Type

interface TypeNumber : Type

class TypeReal : TypeNumber

class TypeFloat : TypeNumber

open class TypeNatural : TypeNumber

class TypeNatural1 : TypeNatural()

class TypeInteger : TypeNumber {
    override fun equals(other: Any?): Boolean {
        return other is TypeInteger
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class TypeBoolean(val value: BooleanValue? = null) : Type

class TypeString : Type

class TypeVoid : Type

class TypeCollection(val type: CollectionType, val name: String = "") : Type {
    override fun equals(other: Any?): Boolean {
        if (other !is TypeCollection) return false
        return type == other.type && name == other.name
    }

    override fun hashCode(): Int {
        return Objects.hash(type, name)
    }
}

class TypeAnonymousCollection(val type: CollectionType) : Type {
    override fun equals(other: Any?): Boolean {
        if (other is TypeAnonymousCollection) return type == other.type
        if (other is TypeCollection) return type == other.type
        return false
    }

    override fun hashCode(): Int {
        return Objects.hash(type)
    }
}

class TypeFunction(val type: FunctionType) : Type

class TypeSet(val type: SetType) : Type

class TypeInterval(val type: TypeNumber) : Type

class TypeCouple : Type

enum class SetType {
    INTEGER,
    NATURAL,
    NAT,
    BOOL
}

enum class BooleanValue {
    TRUE,
    FALSE
}

class UnknownTypeException(msg: String) : Exception("Unknown type $msg")

class InvalidTypeException(msg: String) : Exception(msg)