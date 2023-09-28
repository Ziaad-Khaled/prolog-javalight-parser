# prolog-javalight-parser

## Project Description

The JavaLight Parser is a Prolog-based parser for a definite clause grammar of Java-Light. JavaLight is a simplified fragment of Java, consisting of non-empty semi-colon-separated sequences of grammatical Java statements. The parser supports the following Java-Light statements:

1. **Assignment Statements:** 
   - The left side of the assignment is a Java identifier, and the right side is an arithmetic expression involving binary operators (+, -, *, /, %).
   - Expressions can have parenthesized sub-expressions, and atomic sub-expressions are identifiers or unsigned integer literals.

   Example:
   ```java
   counter1 = counter1 + (x / y - 21) % _w2;
   ```

2. **Conditional Statements:**

    - Conditional statements include valid Java **'if'** and **'if-else'** statements, nested to any depth, with single-statement bodies.
    - Conditions are simple relational expressions involving operators (==, !=, <=, <, >=, >) flanked by arithmetic expressions.

    Example:
    ```java
    if (counter > w + 2) {
    if (counter > x) {
        counter = y;
    } else {
        if (counter > y) {
            counter = x;
        }
        w = y / x;
      }
    }
    ```

3. **Loops:**

    - Loops consist of valid Java while loops, nested to any depth, with conditions as described above, and single-statement bodies.

    Example:
   ```java
   counter = x + y;
   while (counter <= w - 1) {
      while (counter != y) {
          counter = counter + x + 5;
      }
      if (counter > w + 2) {
          if (counter > x) {
              counter = y;
          } else {
              if (counter > y) {
                  counter = x;
              }
              w = y / x;
            }
        }
    }
   ```

## Parsing and Parse Tree Generation

This parser uses Prolog to analyze Java-Light code and generate parse trees for the input. The parse trees are linearly represented, with labels for leaves and parent nodes.

  - Leaf nodes are represented as l, where l is the label of the leaf.
- Parent nodes are represented as `p(l₁; l₂; ...; lₙ)`, where p is the label of the parent node, and `li` is the ith sub-tree.

## Query Format

To ensure the soundness and completeness of your grammar, queries should be of the following format:
```prolog
s(T, [c, o, u, n, t, e, r, =, 0, ;], []).
```
Make sure that queries in this format run correctly.

## Usage

To use the JavaLight Parser, follow these steps:

  - Clone this repository to your local machine.
  - Ensure you have a Prolog interpreter installed.
 - Run the parser on your Java-Light code to generate parse trees.
