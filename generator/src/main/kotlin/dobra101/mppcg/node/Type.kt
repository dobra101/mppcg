package dobra101.mppcg.node

import dobra101.mppcg.node.b.FunctionType
import dobra101.mppcg.node.collection.AnonymousCollectionType
import dobra101.mppcg.node.collection.CollectionType

interface Type

interface TypeNumber : Type

class TypeReal : TypeNumber

class TypeInteger : TypeNumber {
    override fun equals(other: Any?): Boolean {
        return other is TypeInteger
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class TypeString : Type

class TypeVoid : Type

class TypeCollection(val name: String, val type: CollectionType) : Type

class TypeAnonymousCollection(val type: AnonymousCollectionType) : Type

class TypeFunction(val type: FunctionType) : Type

class TypeSet(val type: SetType) : Type

class TypeInterval(val type: TypeNumber) : Type

enum class SetType {
    INTEGER,
    NATURAL,
    NAT,
    BOOL
}

class UnknownTypeException(msg: String): Exception("Unknown type $msg")