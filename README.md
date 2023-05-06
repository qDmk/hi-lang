# Hi-lang

Simple **REPL** for interpreted language *Hi*  
Written as a homework for **Haskell** course in university  
**Assignment** can be found [here](/task.md)

### Functionality
- Numbers and booleans, arithmetic functions and infix operatros for them
- Strings, slices
- Lists, folds and slices
- Byte-strings, serialisation and compression
- Dictionaries and function for them
- File I/O, directory navigation
- Data/Time and random values

### Structure
- [Parser](src/HW3/Parser.hs) using [megaparsec](https://hackage.haskell.org/package/megaparsec) monadic parser combinators
- [Evaluator](src/HW3/Evaluator.hs)
- [CLI](src/HW3/Pretty.hs) using [prettyprinter](https://hackage.haskell.org/package/prettyprinter)
