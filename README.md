# MPPCG

This is **MPPCG**, a multi programming paradigm code generator.
It separates the output languages to enable enhanced extendability and maintainability.
**MPPCG** is designed to support not only output languages of different programming paradigms,
but also different input languages.
However, at the current state, only models of the B-Method are supported as
input code.
Supported output languages are currently Java and Prolog.

## Structure
```
|-- mppcg
    |-- inputLanguage
        |-- <inputLanuageName>
            |-- <outputLanguage_1> (e.g. BTypes in Java)
            |-- <outputLanguage_n> (e.g. BTypes in Prolog)
    |-- outputLanguage
        |-- <outputLanguage_1> (e.g. JavaOutputEnvironment and templates)
        |-- <outputLanguage_n> (e.g. PrologOutputEnvironment and templates)
    |-- generator
    |-- src (contains Launcher and input files)
```

## Generate Code
To generate code, navigate to the root folder end execute
`gradle run --args="--lang <language> --file <filename>"`

Example calls for benchmarked machines:

`gradle run --args="--lang prolog --file CAN_BUS_tlc"`

`gradle run --args="--lang java --file CAN_BUS_tlc"`

`gradle run --args="--lang prolog --file Train_1_beebook_deterministic_MC"`

`gradle run --args="--lang java --file Train_1_beebook_deterministic_MC"`



***Important***: Currently, the machine files have to be located in [src/main/resources/machines](src/main/resources/machines)

The files will then be generated to `generator/build/generated/`

After generation, the run config and required libraries of the *input language module* are also copied to `generator/build/generated`.

## Running Tests
To run tests, the environment variable `jasper` need to be set.
This points to the `jasper.jar`-file used in Prolog tests.

Example: `/usr/local/sicstus4.7.0/lib/sicstus-4.7.0/bin/jasper.jar`

On MacOS, Intellij has sometimes problems accessing environment variables.
For a workaround, the path can be hardcoded in [build.gradle.kts](build.gradle.kts)