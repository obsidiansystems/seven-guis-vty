# seven-guis-vty

Repository for the [7GUIs](https://eugenkiss.github.io/7guis/) project.

## The structure of tutorials

Tutorials are packaged, like most Haskell applications and libraries, as a [cabal](https://www.haskell.org/cabal/) package. It was created by running `cabal init`. One thing that is a little unusual, though not unheard of, is that the source files for this package are [literate Haskell](https://wiki.haskell.org/Literate_programming) files. Everything outside of code blocks that begin with "\`\`\`haskell" will be ignored by the compiler.

We use the [markdown-unlit](https://github.com/sol/markdown-unlit) preprocessor to allow us to write tutorials in markdown (rather than one of Haskell's built-in literate document styles) but still allow the code blocks to be recognized and compiled.

Each of the code snippets will be a function that can be run on its own in the REPL. The functions will gradually grow in complexity until the final version implements the desired GUI. This isn't how you'd normally program, but we're trying to "show our work" so that you can see how we build up the GUI in smaller steps.

> NB: One limitation of the format we've chosen (a single literate haskell source file) is that the module imports have to be declared in one place, above all of the code, so the first code you see will include imports that aren't needed until later. We'll use qualified imports to try to make it clear why something is being imported.

## Blog Posts

1. [Counter](https://obsidian.systems/blog/seven-guis-vty-1-counter)
1. [Temperature-Converter](https://blog.obsidian.systems/seven-guis-in-reflex-vty-part-2-the-temperature-converter/)
