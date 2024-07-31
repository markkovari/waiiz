# What is this?

This is the zig version of the Write an interpreter in Go book by Thorsten Ball.
[Link to the book](https://interpreterbook.com/) if you haven't read it yet ... go read it!


## Why?

I already finished the first book and I wanted to recreate the interpreter in zig, because wanted to learn zig and I thought it would be a fun project. Meanwhile I am paralelly doing the second book [Writing a compiler in Go](https://compilerbook.com/) in go.


# Setup

with [mise](https://mise.jdx.dev/configuration.html)

then run:

```bash
mise install
```


# Running the tests

```bash
zig build test --summary all --verbose

## please don't forget to include the new files in the build.zig file later me
```


and you kinda need vscode to have ZLS and ZIG paths enabled, sorry (neo)vim fans.

# Contributing

Feel free to create PRs and issues, but I will definitely not prioritize them. Cheers! 🍻
Hopefully you will learn some new things as I did.