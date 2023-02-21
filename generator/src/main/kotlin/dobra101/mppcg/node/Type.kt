package dobra101.mppcg.node

import dobra101.mppcg.node.b.FunctionType
import dobra101.mppcg.node.collection.AnonymousCollectionType
import dobra101.mppcg.node.collection.CollectionType
import java.util.Objects

interface Type

interface TypeNumber : Type

class TypeReal : TypeNumber

class TypeNatural : TypeNumber

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

class TypeAnonymousCollection(val type: AnonymousCollectionType) : Type {
    override fun equals(other: Any?): Boolean {
        if (other !is TypeAnonymousCollection) return false
        return type == other.type
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

class UnknownTypeException(msg: String): Exception("Unknown type $msg")

class InvalidTypeException(msg: String): Exception(msg)