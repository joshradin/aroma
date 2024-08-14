# `aroma_ast_parsing`

Parses code into an aroma ast.

```rust
static AROMA_CODE: &'static str = r#"
using aroma.ops.Comparable;

trait Animal {
    fn sayHi();
}

const dog = new Animal() {
    fn sayHi() {
        prinln("hi doggy);
    }
}.mixin (Comparable<Animal>) {
    fn compare(other: Animal) -> Int {
        retuth this == other ? 0 : -1;
    }
};

const cat =  new Animal() {
    fn sayHi() {
        prinln("hi cat");
    }
};

dog.compare(cat); // ok because dog has Comparable<Animal> mixin
cat.compare(dog); // type error!
"#;
```