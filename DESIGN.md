# MiniZinc-Lite — Design and Implementation Documentation

## 1. Overview

MiniZinc-Lite is a constraint-solving application written primarily in Haskell, with a TypeScript frontend. It accepts a restricted subset of MiniZinc, parses the input into an Abstract Syntax Tree (AST), and searches for assignments satisfying the declared constraints.

The implementation separates the application into three main components:

- **Parser:** Converts MiniZinc text into a structured AST.
- **Solver:** Evaluates expressions and searches for valid or optimal assignments.
- **Web interface:** Sends models to the Haskell backend and displays or saves results.

The design emphasizes pure functions, algebraic data types, parser combinators, immutable search states, and reusable abstractions.

## 2. Abstract Syntax Tree Design

The AST provides an intermediate representation between parsing and solving.

The main `ADT` type stores declarations, constraints, and a solve directive.

Declarations distinguish integer constants, scalar decision variables, and array decision variables.

Expressions represent integer literals, identifier references, indexed array accesses, arithmetic operations, and numeric macros.

Constraints represent comparisons and predefined boolean macros.

The design uses algebraic data types rather than evaluating expressions while parsing. This makes the intermediate representation easier to inspect, test, and extend.

All custom types used in the AST derive the standard `Show` instance to support debugging and automated assessment.

## 3. Parser Architecture

The parser uses the combinator infrastructure provided in the assignment scaffold.

Small parsers recognize individual grammatical constructs, including identifiers, signed integers, domains, operators, and delimiters.

Larger parsers combine these components using `Functor`, `Applicative`, `Alternative`, and `Monad` operations.

Examples include:

- `<$>` to construct AST nodes from parsed values.
- `<*>` to combine independent parser results.
- `<|>` to express alternative grammatical forms.
- `many` to recognize repeated declarations and constraints.
- `do` notation to sequence dependent parsing operations.

Arithmetic parsing is organized into precedence levels. Exponentiation is right-associative, while addition, subtraction, multiplication, and division are left-associative.

The parser requires a single final solve directive and complete input consumption.

### 3.1 Semantic Validation

Semantic validation checks declaration and reference rules.

These include duplicate identifiers, invalid integer ranges, invalid array lengths, and references to undefined identifiers.

Previously declared constants are evaluated and reused when constructing constant-dependent integer domains.

Generator variables are local to their associated generator expressions and must not be confused with globally declared identifiers.

## 4. Constraint Solver Design

The solver uses recursive backtracking over finite decision-variable domains.

Each search branch constructs an updated immutable search state containing variable bindings, constant bindings, remaining declarations, and constraints.

The solver evaluates expressions through `Maybe` to represent operations that may fail.

Invalid operations, such as division by zero or unavailable array elements, produce `Nothing` instead of causing an exception.

### 4.1 Incremental Constraint Checking

Constraints are checked when the information required for their evaluation becomes available.

If a fully evaluable constraint is false, the solver rejects the current branch immediately.

This avoids exploring unnecessary assignments and improves efficiency compared with checking constraints only after every variable has been assigned.

### 4.2 Optimisation

The application supports three solve directives:

- `solve satisfy;`
- `solve minimize expression;`
- `solve maximize expression;`

Satisfaction returns a valid assignment.

Optimisation evaluates the objective for valid assignments and selects one with the smallest or largest objective value.

## 5. Constraint Macros

Numeric macros include `sum`, `min`, and `max`.

Boolean macros include `all`, `any`, and `allDifferent`.

Macro inputs may be written as literal lists, array references, or generator expressions where supported.

Using shared representations for macro inputs reduces duplication between parsing and evaluation.

## 6. Arrays and Generators

Array declarations represent one-based collections of decision variables.

The solver expands array declarations into individual variable bindings, following the naming convention `q[1]`, `q[2]`, and so on.

Indexed expressions evaluate the corresponding array element.

Generator expressions produce values by iterating over a finite domain and evaluating their bodies under temporary iterator bindings.

This allows models such as N-Queens to express row and diagonal restrictions using `allDifferent`.

Partial `allDifferent` checking can reject repeated known values before all array elements have been assigned.

## 7. Frontend and Backend Integration

The frontend is implemented in TypeScript using RxJS and Vite.

Observable streams capture model edits and button clicks.

The Haskell backend exposes HTTP endpoints for solving models and saving solutions.

The Save feature uses Haskell file I/O to write assignments to timestamped `.out` files.

The output contains a generation timestamp followed by one variable assignment per line.

This design keeps the browser responsible for user interaction while the backend handles parsing, solving, and file generation.

## 8. Functional Programming Design Decisions

Several functional programming principles guide the implementation:

**Pure functions:** Parsing logic, expression evaluation, constraint checking, and search-state transformations are separated from input/output operations.

**Immutable data:** Backtracking constructs new search states rather than modifying shared variable assignments.

**Pattern matching:** AST constructors are handled through explicit cases, keeping the evaluation logic structured and readable.

**Higher-order functions:** Standard operations such as `map`, `filter`, `foldl`, `traverse`, and `all` are reused instead of implementing imperative loops.

**Algebraic data types:** Different grammatical constructs are represented through explicit constructors, enabling modular extension.

**Failure handling:** `Maybe` represents potentially invalid computations without requiring exceptions for ordinary invalid assignments.

## 9. MiniZinc-Lite Grammar (BNF-Style Extension)

The following grammar documents the principal language constructs. It is intended as an explanatory grammar rather than a fully formal specification of whitespace, semantic checks, or operator precedence.

```text
<model> ::= <declaration>* <constraint>* <solve>

<declaration> ::= <constant-declaration>
                | <variable-declaration>
                | <array-declaration>

<constant-declaration> ::= "int" ":" <identifier>
                           "=" <expression> ";"

<variable-declaration> ::= "var" <domain>
                           ":" <identifier> ";"

<array-declaration> ::= "array" "[" "1" ".." <bound> "]"
                        "of" "var" <domain>
                        ":" <identifier> ";"

<domain> ::= <bound> ".." <bound>
           | "{" <integer-list> "}"

<constraint> ::= "constraint" <comparison> ";"
               | "constraint" <boolean-macro> ";"

<comparison> ::= <expression> <comparison-op> <expression>

<comparison-op> ::= "=" | "!=" | "<" | "<="
                  | ">" | ">="

<solve> ::= "solve" "satisfy" ";"
          | "solve" "minimize" <expression> ";"
          | "solve" "maximize" <expression> ";"

<expression> ::= <integer>
               | <identifier>
               | <identifier> "[" <expression> "]"
               | "(" <expression> ")"
               | "-" <expression>
               | <expression> <arithmetic-op> <expression>
               | <numeric-macro>

<arithmetic-op> ::= "+" | "-" | "*" | "/" | "^"

<numeric-macro> ::= <numeric-name> "(" <macro-source> ")"

<numeric-name> ::= "sum" | "min" | "max"

<boolean-macro> ::= <boolean-name> "(" <macro-source> ")"

<boolean-name> ::= "all" | "any" | "allDifferent"

<macro-source> ::= "[" <expression-list> "]"
                 | <identifier>
                 | <identifier> "in" <domain>
                   ")" "(" <expression>

<expression-list> ::= <expression>
                    | <expression> "," <expression-list>
```

Operator precedence and associativity are defined by the expression parser rather than the simplified grammar above. Boolean macro generators use comparison expressions where required.

## 10. Testing Strategy

Testing covers the parser, expression evaluation, constraint solving, optimisation, array handling, and integration.

Representative cases include:

- Valid and invalid integer domains.
- Duplicate and undefined identifiers.
- Arithmetic precedence and exponentiation associativity.
- Division by zero.
- Satisfiable and unsatisfiable constraints.
- Numeric and boolean macros.
- Array element indexing.
- Generator expressions.
- Four-Queens and Eight-Queens models.
- Frontend Run and Save workflows.

Automated regression tests supplement the provided assignment tests. Compilation and runtime checks should be performed using `stack build`, `stack test`, `stack run`, and `npm run dev`.

## 11. Limitations

MiniZinc-Lite implements only the restricted language subset required for the assignment.

The backtracking solver is intended for finite, reasonably sized domains rather than industrial-scale constraint problems.

It does not provide the advanced propagation, heuristics, and optimisation algorithms found in full MiniZinc solvers.

The BNF-style grammar is explanatory and does not capture all semantic validation requirements.

## 12. AI Assistance and Attribution

ChatGPT was used during development to suggest implementation approaches, parser and solver code, tests, debugging changes, frontend integration, and documentation.

The final submitted implementation was reviewed, tested, and understood by me.