package dobra101.mppcg.node

import dobra101.mppcg.node.b.FunctionType

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

class TypeEnum(val name: String) : Type

class TypeFunction(val type: FunctionType) : Type

class TypeSet(val type: SetType) : Type

class TypeInterval(val type: TypeNumber) : Type

enum class SetType {
    INTEGER,
    NATURAL,
    NAT,
    BOOL
}