# Seven GUIs in reflex-vty, Part 2: The Temperature Converter

> 7GUIs defines seven tasks that represent typical challenges in GUI programming
> -- [7GUIs: A GUI Programming Benchmark](https://eugenkiss.github.io/7guis/)

[7GUIS](https://eugenkiss.github.io/7guis/) is a set of 7 typical GUI programming tasks of varying levels of complexity. We're going to implement the 7GUIs in [Haskell](https://haskell.org) using the [reflex](https://reflex-frp.org) [functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) framework.

## The structure of this document

This tutorial is packaged, like most Haskell applications and libraries, as a [cabal](https://www.haskell.org/cabal/) package. It was created by running `cabal init`. One thing that is a little unusual, though not unheard of, is that the source file for this package is a [literate Haskell](https://wiki.haskell.org/Literate_programming) file. Everything outside of code blocks that begin with "\`\`\`haskell" will be ignored by the compiler.

We use the [markdown-unlit](https://github.com/sol/markdown-unlit) preprocessor to allow us to write the tutorial in markdown (rather than one of Haskell's built-in literate document styles) but still allow the code blocks to be recognized and compiled.

Each of the code snippets will be a function that can be run on its own in the REPL. The functions will gradually grow in complexity until the final version implements the desired GUI. This isn't how you'd normally program, but we're trying to "show our work" so that you can see how we build up the GUI in smaller steps.

> NB: One limitation of the format we've chosen (a single literate haskell source file) is that the module imports have to be declared in one place, above all of the code, so the first code you see will include imports that aren't needed until later. We'll use qualified imports to try to make it clear why something is being imported.

## Setting up and building

### Getting started with reflex-platform and nix

We're going to use [reflex-platform](https://github.com/reflex-frp/reflex-platform) and [nix](https://nixos.org/nix) to build this project. The readme of the former explains why:

> Reflex Platform is a curated package set and set of tools that let you build Haskell packages so they can run on a variety of platforms. Reflex Platform is built on top of the nix package manager.
> There are five main reasons to use Reflex Platform:
>
> 1. It's curated: the core packages in Reflex Platform are known to work together and are tested together.
> 2. It's cached: the core packages in Reflex Platform are cached so you can download prebuilt binaries from the public cache instead of building from scratch.
> 3. It's consistent: nix locks down dependencies even outside the Haskell ecosystem (e.g., versions of C libraries that the Haskell code depends on), so you get completely reproducible builds.
> 4. It's cross-platform: Reflex Platform is designed to target iOS and Android on mobile, JavaScript on the web, and Linux and macOS on desktop. It's Haskell, everywhere.
> 5. It's convenient: Reflex Platform comes packaged with tools to make development easier, like a hoogle server that you can run locally to look up definitions.

To set up reflex-platform and nix, follow the directions [here](https://github.com/reflex-frp/reflex-platform#setup) and then come back and we can get started.

### Running this project

We use reflex-platform to build an environment that contains all the dependencies you need to build this project. You can either compile this project and run the resulting executable, or open it in an interpreter and run it from there. The latter makes for a quicker development cycle if you're modifying any of the example code.

#### The development environment

To enter a shell from which you can build the project or enter the REPL, run `nix-shell` from the project directory.

You'll be put in a nix-shell that looks something like this:

```bash
[nix-shell:/path/to/project]$
```

This tells you that you're in a "nix-shell" environment. Inside this environment, the dependencies for our project are ready for us to use.

From within the nix-shell, you can run:

* `cabal repl` to enter a REPL
* `cabal build` to build the project
* `ghcid` to watch source files and display any errors or warnings that arise when they change

From within the REPL, you can run any of the functions defined later in this tutorial (e.g., `hello`) to see what it does.

#### nix-build

To compile the project via nix, run `nix-build` from the project directory. This will create a `result` symlink in your project folder. Run the program by executing `result/bin/seven-guis-vty-counter`.

Enough with the preliminaries; let's get on to the code.

## The code

### Imports

As mentioned above, we'll start by importing the libraries our project depends on. Don't worry if you don't understand why something is in the import list: we'll explain that as we go.

```haskell
-- These imports are unqualified because we'll be using them heavily
import Reflex
import Reflex.Vty

import qualified Graphics.Vty as V
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Text.Read as Read
```

### Starting up a reflex-vty application

> This could be written in other ways, but we're going to try to keep the code as plain and explicit as possible here.

Now, if you go into your REPL and run `aWayOut`, you should see a ... blank screen. Not very exciting. What is exciting, though, is that you can escape that blank screen and get back to the REPL by pressing `Ctrl+c`.

> As an exercise, you could try to add an alternative key combination that also exits the program. How about `Esc`?

## The temperature converter GUI

```haskell
counter :: IO ()
counter = mainWidget $ initManager_ $ do
  getout <- ctrlc
  tile flex $ box (pure roundedBoxStyle) $ row $ do
    rec grout flex $ text numClicksText
        buttonClicked <- tile flex $ textButton def "Count"
        numClicks <- count buttonClicked
        let numClicksText = current $ fmap (T.pack . show) numClicks
    return ()
  return $ fmap (\_ -> ()) getout
```

```haskell
multipleTextLayouts :: IO ()
multipleTextLayouts = mainWidget $ initManager_ $ do
  getout <- ctrlc
  tile flex $ box (pure roundedBoxStyle) $ row $ do
    rec tile flex $ textInput def
        grout flex $ text "Celsius ="
        tile flex $ textInput def
        grout flex $ text "Fahrenheit"
    return ()
  return $ fmap (\_ -> ()) getout
```

```haskell
toC :: Double -> Double
toC c = (c - 32) * (5 / 9)

toF :: Double -> Double
toF f = f * (9 / 5) + 32

toNumber :: Z.TextZipper -> Maybe Double
toNumber tz = Read.readMaybe (T.unpack (Z.value tz))

toTZ :: Double -> Z.TextZipper
toTZ val = Z.fromText (T.pack (show val))
```

```haskell
celsiusToFahrenheit :: IO ()
celsiusToFahrenheit = mainWidget $ initManager_ $ do
  getout <- ctrlc
  tile flex $ box (pure roundedBoxStyle) $ row $ do
    rec celsiusInput <- tile flex $ textInput def
        let celsiusEv = _testInput_updated celsiusInput
        grout flex $ text "Celsius ="
        let setFahrenheitEvent =
              fforMaybe celsiusEv $ \val ->
                case toNumber val of
                  Nothing -> Nothing
                  Just num -> Just (toTZ (toF num))
        fahrenheitInput <- tile flex $ textInput def
            { _testInputConfig_setValue = Just setFahrenheitEvent
            }
        grout flex $ text "Fahrenheit"
    return ()
  return $ fmap (\_ -> ()) getout
```


```haskell
bijectiveTemperature :: IO ()
bijectiveTemperature = mainWidget $ initManager_ $ do
  getout <- ctrlc
  tile flex $ box (pure roundedBoxStyle) $ row $ do
    rec celsiusInput <- tile flex $ textInput def
            { _testInputConfig_setValue = Just setCelsiusEvent
            }
        let celsiusEv = _testInput_updated celsiusInput
        grout flex $ text "Celsius ="
        let setCelsiusEvent =
              fforMaybe fahrenheitEv $ \val ->
                case toNumber val of
                  Nothing -> Nothing
                  Just num -> Just (toTZ (toC num))
        let setFahrenheitEvent =
              fforMaybe celsiusEv $ \val ->
                case toNumber val of
                  Nothing -> Nothing
                  Just num -> Just (toTZ (toF num))
        fahrenheitInput <- tile flex $ textInput def
            { _testInputConfig_setValue = Just setFahrenheitEvent
            }
        let fahrenheitEv = _testInput_updated fahrenheitInput
        grout flex $ text "Fahrenheit"
    return ()
  return $ fmap (\_ -> ()) getout
```

```haskell
convertEvent :: Reflex t => (Double -> Double) -> Event t Z.TextZipper -> Event t Z.TextZipper
convertEvent conv ev =
  fforMaybe ev $ \val ->
    case toNumber val of
      Nothing -> Nothing
      Just num -> Just (toTZ (conv num))
```

```haskell
thisIsIt :: IO ()
thisIsIt = mainWidget $ initManager_ $ do
  getout <- ctrlc
  tile flex $ box (pure roundedBoxStyle) $ row $ do
    rec
        let setCelsiusEvent = convertEvent toC (_testInput_updated fahrenheitInput)
        let setFahrenheitEvent = convertEvent toF (_testInput_updated celsiusInput)
        celsiusInput <- tile flex $ textInput def
            { _testInputConfig_setValue = Just setCelsiusEvent
            }
        grout flex $ text "Celsius ="
        fahrenheitInput <- tile flex $ textInput def
            { _testInputConfig_setValue = Just setFahrenheitEvent
            }
        grout flex $ text "Fahrenheit"
    return ()
  return $ fmap (\_ -> ()) getout
```

```haskell
main :: IO ()
main = thisIsIt
```
