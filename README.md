# The Saber Programming Language

A research functional programming language written in Gleam!

The long term goal is a language with very different syntax and type system that is compiled, not interpreted.
This interpreter exists to explore the frontend features (syntax and type system) for ergonomics.
The current type system is a simple quasi-static (a la Thatte) system-F with basically no type inference.
The immediate plan is to switch to a novel PTS I call System-L2omega (but only quasi-static, for developer convenience), 
and then frame it in quantified type theory after that.
In terms of inference, there is still no plan for adding more type inference. However, implicit arguments is a plan. 
For example, `def id = fn<a: Type>(x: a) x` being called as `id(7)`, instead of `id<Int>(7)`.
There are, of course, many basic features missing. Sum-of-product type definitions are high priority, as well as a module system.
IO is also a priority, though must come after the upcoming type system.
Though this is an interpreted language, it is designed as if it were compiled to machine code, with restrictions on language features to reflect that plan.

## Quick start

Saber code will be executed from the `main.sb` file, and the value of the last declaration will be printed to the console.

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
