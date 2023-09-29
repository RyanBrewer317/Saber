# The Saber Programming Language

A research functional programming language written in Gleam!

The long term goal is a language with very different syntax and type system that is compiled, not interpreted.
This interpreter exists to explore the frontend features (syntax and type system) for ergonomics.
The current type system is a quasi-static (a la Thatte) System-F_omega with multi-parameter functions (not auto-curried) and
implicit type arguments. In addition, it is extended with a novel calculus of labels, but there's nothing in place yet to
make use of this or show it off. The long term plan is to frame this in Quantified Type Theory (QTT) as well. 
More immediately, the plan is to add a two-tiered module system (compilation units called "libraries" with modules inside),
then ADTs (no pattern matching), then a WASM compiler backend, then I/O constructs, then binary operators, then control flow.
The syntax is also very much experimental, and likely to change dramatically to account for considerations such as parallel parsing.

```
// a polymorphic identity function
// `a` is an implicit parameter and must appear in the types of
// explicit parameters so it can be inferred at the callsite.
def id = fn<a>(x: a) x
def demo1 = id(id)(id(4)) // evaluates to 4

// a slightly fancier example
// implicit parameters without type annotations are given the type `type`
// explicit parameters without type annotations are given the type `dyn`
def first = fn<a, b>(x: a, y: b) x
def demo2 = first(id(7), fn(x) 4) // a=int, b=dyn->int, evaluates to 7
```

Though this is an interpreted language, it is designed as if it were compiled to machine code, 
with restrictions on language features to reflect that plan.

## Quick start

Saber code will be executed from the `main.sb` file, and the value of the last declaration will be printed to the console. 
It uses the javascript runtime.

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
