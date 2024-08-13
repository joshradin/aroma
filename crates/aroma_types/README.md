# `aroma_types`

Aroma type system, describing how types works and an abstraction over the facilities
provided by the [`aroma_bytecode`](aroma_bytecode) crate.

## Objects

The aroma type system is single inheritance with mixins. All types within aroma are objects with are instances of some
`class`, which means they all inherit from the
`aroma.system.Object` class. This includes primitive types, which shall be wrapped
when necessary. In additions, types can implement mixins known as `traits`. In addition, one can define `abstract class`
types that are a mix of `class` and `trait` definitions that can not be directly instantiated.

## Primitives

Primitives are wrapped in objects, and automatically compiled to primitives when allowed.

- `aroma.system.Byte` -> `i8`
- `aroma.system.Int` -> `i32`
- `aroma.system.Long` -> `i64`
- `aroma.system.Boolean` -> `bool`
- `aroma.system.Float` -> `f32`
- `aroma.system.Double` -> `f64`
