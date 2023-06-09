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

Example:
`gradle run --args="--lang prolog --file TrafficLight_MC"`

Currently, the files have to be located in [src/main/resources/machines](src/main/resources/machines)

The files will then be generated to `generator/build/generated/`