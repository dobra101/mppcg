package dobra101.mppcg.node

import dobra101.mppcg.node.b.FunctionType

interface Type

interface TypeNumber : Type

class TypeDouble : TypeNumber

class TypeInteger : TypeNumber

class TypeString : Type

class TypeVoid : Type

class TypeFunction(val type: FunctionType) : Type

class TypeSet(val type: SetType) : Type

class TypeInterval(val type: TypeNumber) : Type

enum class SetType {
    INTEGER,
    NATURAL,
    NAT,
    BOOL
}