# Monadic Parser for Logic Expressions

This project implements a monadic parser for logic expressions in Haskell.

## Context-Free Grammar

The logic context-free grammar used in this project is defined as follows:

```
File = Expression
Expression = Disjunction | Disjunction -> Expression
Disjunction = Conjunction | Disjunction | Conjunction
Conjunction = Negation | Conjunction & Negation
Negation = !Negation | Variable | (Expression)
Variable = [A..Z][A..Z|0..9|']*
```

## Usage

To use the monadic parser for logic expressions, follow these steps:

1. **Clone the Repository**:

   ```bash
   git clone https://github.com/fedorbondarev/haskell_expressions_parser.git
   ```

2. **Navigate to the Project Directory**:

   ```bash
   cd haskell_expressions_parser
   ```

3. **Run the Parser**:

   Use the parser module in your Haskell code to parse logic expressions according to the specified grammar.

## Example

```haskell
import Parser

main :: IO ()
main = do
  let input = "A | (B & C) -> D"
  case runParser parseFile input of
    Just expr -> putStrLn $ "Parsed expression: " ++ show expr
    _ -> putStrLn "Parse error"
```