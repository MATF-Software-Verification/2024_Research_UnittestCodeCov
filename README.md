# FMutant - F# Mutation Testing Engine

A mutation testing tool for F# that evaluates test suite quality by introducing systematic code mutations and verifying that tests detect them.

## What is Mutation Testing?

Mutation testing is a technique to assess the **quality** of your test suite, not just coverage. It works by:

1. Introducing small bugs (mutations) into your code
2. Running your test suite against each mutation
3. Checking if tests catch the mutations

**Key Metrics:**

- **Killed mutation**: Tests failed (good - mutation detected)
- **Survived mutation**: Tests passed (bad - reveals test gap)
- **Mutation score**: (Killed / Total) × 100%

A high mutation score indicates a robust test suite that can catch real bugs.

## Quick Start

```bash
# Build the project
dotnet build

# Run mutation testing on a file
dotnet run Sample.fs

# Select a function from the interactive menu
# Example output:
# 1) add
# 2) isLessThanConst
# ...
# Select output: 1
```

## Features

- **10 Mutation Operators**: Boolean flips, integer mutations, arithmetic/logical operator swaps
- **Parallel Execution**: Tests multiple mutations concurrently (configurable)
- **Safe File Operations**: Automatic backup/restore ensures source files are never corrupted
- **Interactive CLI**: Select functions to test from a menu
- **Comprehensive Reporting**: Detailed statistics on mutation survival rates
- **Timeout Handling**: Prevents infinite loops from stalling the test suite (30s default)

## Mutation Operators

| Operator          | Mutation       | Example                |
| ----------------- | -------------- | ---------------------- |
| **BoolFlip**      | `true ↔ false` | `if true` → `if false` |
| **IntPlusOne**    | `n → n+1`      | `x + 5` → `x + 6`      |
| **IntMinusOne**   | `n → n-1`      | `x + 5` → `x + 4`      |
| **IntZero**       | `n → 0`        | `x + 5` → `x + 0`      |
| **IntNegative**   | `n → -n`       | `x + 5` → `x + -5`     |
| **OpPlusToMinus** | `+ → -`        | `a + b` → `a - b`      |
| **OpMinusToPlus** | `- → +`        | `a - b` → `a + b`      |
| **OpMulToDiv**    | `* → /`        | `a * b` → `a / b`      |
| **OpDivToMul**    | `/ → *`        | `a / b` → `a * b`      |
| **OpAndToOr**     | `&& → \|\|`    | `x && y` → `x \|\| y`  |
| **OpOrToAnd**     | `\|\| → &&`    | `x \|\| y` → `x && y`  |

## Example: Strong vs Weak Tests

### Strong Test Suite (100% Mutation Score)

**Function:**

```fsharp
let add a b = a + b
```

**Tests:**

```fsharp
[<Test>]
member _.``add - adds two positive numbers``() =
    Assert.That(add 2 3, Is.EqualTo(5))

[<Test>]
member _.``add - adds zero``() =
    Assert.That(add 10 0, Is.EqualTo(10))
    Assert.That(add 0 0, Is.EqualTo(0))

[<Test>]
member _.``add - adds negative numbers``() =
    Assert.That(add -2 -3, Is.EqualTo(-5))
```

**Result:**

```
Mutation Score: 100.00%
All mutations killed ✓
```

---

### Weak Test Suite (Low Mutation Score)

**Function:**

```fsharp
let survivorFunction x = x * 5 + 0
```

**Test:**

```fsharp
[<Test>]
member _.``survivorFunction - returns 0 for input 0``() =
    Assert.That(survivorFunction 0, Is.EqualTo(0))
```

**Problem:** Testing only with `x=0` means mutations like `x * 6 + 0` still produce `0`.

**Result:**

```
Mutation Score: <100%
Some mutations survived ✗
```

**Fix:** Add tests with non-zero values:

```fsharp
Assert.That(survivorFunction 1, Is.EqualTo(5))
Assert.That(survivorFunction 2, Is.EqualTo(10))
```

## Architecture

The project follows a clean layered architecture:

```
Domain → Core → Infrastructure → Reporting → Application
```

### Layers

1. **Domain** ([src/Domain/Types.fs](src/Domain/Types.fs))

   - Pure F# types: `MutationPoint`, `Mutation`, `MutationStatus`
   - No external dependencies

2. **Core** (Business Logic)

   - [AstWalker.fs](src/Core/AstWalker.fs): AST traversal using FSharp.Compiler.Service
   - [MutationOperators.fs](src/Core/MutationOperators.fs): Mutation generation logic
   - [SourceAnalysis.fs](src/Core/SourceAnalysis.fs): Source text/range conversions

3. **Infrastructure** (External I/O)

   - [TestRunner.fs](src/Infrastructure/TestRunner.fs): Executes `dotnet build` and `dotnet test`
   - [FileOperations.fs](src/Infrastructure/FileOperations.fs): Safe file backup/restore
   - [MutationExecutor.fs](src/Infrastructure/MutationExecutor.fs): Orchestrates mutation execution

4. **Reporting** ([src/Reporting/Reporter.fs](src/Reporting/Reporter.fs))

   - Console output formatting
   - Mutation statistics calculation

5. **Application**
   - [Sample.fs](Sample.fs): Example functions for testing
   - [SampleTests.fs](SampleTests.fs): NUnit test fixtures
   - [Program.fs](Program.fs): CLI entry point

## How It Works

### 1. Parse & Discover

```fsharp
let tree, source = parseFile "Sample.fs"
let points = walkFile tree
```

Uses FSharp.Compiler.Service to parse F# code into an AST and find mutation points.

### 2. Generate Mutations

```fsharp
let candidates = points |> List.collect (createCandidates idGen source)
```

Each mutation point generates multiple mutation candidates based on registered operators.

### 3. Execute & Test

```fsharp
for each mutation do
    backup file → apply mutation → compile → run tests → restore file
```

Each mutation is tested independently with automatic file restoration.

### 4. Determine Status

- **Compile Error**: Mutation caused compilation failure
- **Timeout**: Tests didn't complete in 30 seconds
- **Killed**: Tests failed (mutation detected) ✓
- **Survived**: Tests passed (mutation undetected) ✗

### 5. Report Results

```
==========================================
MUTATION TESTING REPORT
==========================================

Total Mutations:     4
Killed:              3
Survived:            1
Compile Errors:      0
Timeouts:            0

Mutation Score:      75.00%
==========================================
```

## Requirements

- .NET 9.0 SDK
- F# 10.0
- NUnit 4.1.0
- FSharp.Compiler.Service 43.10.100

## Project Structure

```
.
├── src/
│   ├── Domain/
│   │   └── Types.fs              # Core domain types
│   ├── Core/
│   │   ├── AstWalker.fs          # AST traversal
│   │   ├── MutationOperators.fs  # Mutation logic
│   │   └── SourceAnalysis.fs     # Range/index conversion
│   ├── Infrastructure/
│   │   ├── FileOperations.fs     # File backup/restore
│   │   ├── TestRunner.fs         # Process execution
│   │   └── MutationExecutor.fs   # Mutation orchestration
│   └── Reporting/
│       └── Reporter.fs           # Console reporting
├── Sample.fs                     # Example functions
├── SampleTests.fs                # NUnit tests
├── Program.fs                    # CLI entry point
└── FMutant.fsproj               # Project file
```

## Limitations

- **Current project scope only**: Files must be part of the current solution
- **Source-level mutation**: Requires recompilation for each mutation (slower than bytecode mutation)
- **Sequential execution**: Parallel execution is implemented but currently disabled for clearer output
- **Limited operators**: Supports 10 common mutation operators (extendable)

## Extending the Tool

### Adding New Mutation Operators

1. Add pattern matching case in [MutationOperators.fs](src/Core/MutationOperators.fs):

```fsharp
match point.NodeKind with
| "Const.Bool" -> mutateBool mutationFor original token
| "Const.Int32" -> mutateInt mutationFor original token
| "Op.Infix" -> mutateInfixOp mutationFor original token
| "YourNewKind" -> mutateYourNew mutationFor original token  // ← Add here
```

2. Implement mutation function:

```fsharp
let private mutateYourNew
    (mutationFor: string -> string -> Mutation)
    (original: string)
    (token: string option)
    : Mutation list =
    // Your mutation logic here
    [ mutationFor "newValue" "YourOperatorName" ]
```

3. Update [AstWalker.fs](src/Core/AstWalker.fs) to detect new AST node types if needed.

## Future Improvements

### 1. Enhanced Mutation Operators
**Current limitation**: 10 basic operators focused on arithmetic, boolean, and logical operations.

**Improvements**:
- **Relational operators**: `<` ↔ `<=`, `>` ↔ `>=`, `=` ↔ `<>`
- **Boundary mutations**: `x < n` → `x <= n-1`, array indices `arr.[i]` → `arr.[i+1]`
- **String mutations**: `"hello"` → `""`, `"hello"` → `"hello2"`
- **Collection mutations**: `List.filter` → `List.map`, `List.head` → `List.last`
- **Return value mutations**: Negate return values, insert early returns
- **Pattern matching mutations**: Swap pattern match cases, remove wildcard cases

**Implementation**: Each new operator requires AST pattern detection in AstWalker and mutation logic in MutationOperators.

### 2. IL/Bytecode Mutation
**Current limitation**: Source-level mutation requires recompilation for every mutation (slow).

**Improvement**: Mutate compiled .NET IL bytecode directly:
```bash
# Compile once
dotnet build → produces Sample.dll

# Mutate IL instructions
IL_0002: add → IL_0002: sub

# Reassemble and test
ilasm mutated.il → run tests on mutated.dll
```

**Benefits**:
- **10-100x faster**: No recompilation needed (single build)
- **Works across languages**: Can test any .NET assembly (C#, VB.NET, F#)
- **Production testing**: Can mutate external compiled libraries

**Trade-offs**:
- Harder to implement (requires IL manipulation via Mono.Cecil)
- Less readable mutations (IL vs. source code)
- Debugging more difficult

**Tools to research**: Mono.Cecil, dnlib, ICSharpCode.Decompiler

### 3. Incremental Mutation Testing
**Current limitation**: Tests all mutations on every run, even if code hasn't changed.

**Improvement**: Only mutate functions modified since last commit:
```bash
# Detect changed functions via git diff
git diff HEAD~1 Sample.fs → find modified line ranges
# Only generate mutations for changed functions
```

**Benefits**:
- **CI/CD integration**: Fast feedback in pull requests
- **Scalability**: Practical for large codebases
- **Developer workflow**: Run incrementally during development

**Implementation**:
- Add git integration to detect changed files/functions
- Cache previous mutation results (JSON file)
- Compare current code hash vs. cached results

### 4. Mutation Prioritization & Sampling
**Current limitation**: Tests every possible mutation (can be slow for large functions).

**Improvement**: Prioritize high-value mutations:
- **Random sampling**: Test 20% of mutations (faster, statistical approximation)
- **History-based**: Focus on mutations that survived in previous runs
- **Complexity-based**: Prioritize mutations in complex code (high cyclomatic complexity)
- **Coverage-based**: Mutate lines with low test coverage first

**Research**: "Selective Mutation Testing" techniques for mutation score estimation

### 5. Advanced Reporting
**Current limitation**: Console-only output with basic statistics.

**Improvements**:
- **HTML reports**: Visual dashboard with syntax-highlighted mutations
  ```
  - Color-coded mutation status (red=survived, green=killed)
  - Interactive diff view (original vs. mutated)
  - Filterable by operator, status, file
  - Trend charts over time
  ```
- **JSON export**: Machine-readable output for CI/CD
  ```json
  {
    "mutationScore": 85.5,
    "mutations": [
      { "id": 1, "status": "Killed", "operator": "OpPlusToMinus" }
    ]
  }
  ```
- **Trend tracking**: Store mutation scores in database/CSV
- **GitHub annotations**: Inline PR comments on surviving mutations

### 6. Equivalent Mutant Detection
**Current limitation**: Some mutations are semantically identical to original code.

**Example**:
```fsharp
// Original
let abs x = if x < 0 then -x else x

// Mutated (hypothetical OpLtToLte)
let abs x = if x <= 0 then -x else x
// For x=0: both return 0 (semantically equivalent)
```

**Problem**: Equivalent mutants can't be killed, lowering mutation score artificially.

**Improvement**: Automatically detect equivalents using:
- **Static analysis**: Compare control-flow graphs
- **Symbolic execution**: Prove semantic equivalence
- **Manual annotation**: Allow developers to mark equivalents
- **TCE (Trivial Compiler Equivalence)**: Compare generated IL

**Research**: "The Equivalent Mutant Problem" (Offutt & Pan, 1997)

### 7. Parallel Execution with Isolation
**Current limitation**: Sequential execution (one mutation at a time).

**Improvement**: True parallel execution with process isolation:
```fsharp
// Create isolated workspaces
/tmp/mutation-1/ → test mutation 1
/tmp/mutation-2/ → test mutation 2
// Run simultaneously without file conflicts
```

**Benefits**:
- **10x speed**: Test 10 mutations simultaneously on 10-core machine
- **Safety**: No risk of concurrent file access
- **Reliability**: Isolated failures don't affect other mutations

**Implementation**: Use `System.IO.Path.GetTempPath()` for temporary workspaces

### 8. Integration with Build Systems
**Current limitation**: Manual CLI invocation only.

**Improvements**:
- **MSBuild task**: Integrate with `dotnet build`
- **dotnet tool**: Install globally via `dotnet tool install -g FMutant`
- **CI/CD plugins**: GitHub Actions, GitLab CI, Azure DevOps
  ```yaml
  # .github/workflows/mutation-test.yml
  - name: Run Mutation Testing
    run: fmutant Sample.fs --threshold 80
  - name: Fail if score below threshold
    run: exit $?
  ```
- **Pre-commit hooks**: Run mutation testing before commits

### 9. Configuration File Support
**Current limitation**: Hardcoded operators, no user configuration.

**Improvement**: Add `.fmutant.json` config file:
```json
{
  "operators": {
    "arithmetic": true,
    "boolean": true,
    "relational": false
  },
  "timeout": 60000,
  "excludePatterns": ["*.Generated.fs", "**/obj/**"],
  "mutationThreshold": 85.0,
  "parallelism": 4
}
```

**Benefits**: Customize behavior per project without code changes

### 10. Test Impact Analysis
**Current limitation**: All tests run for every mutation (even irrelevant ones).

**Improvement**: Only run tests that cover the mutated code:
```fsharp
// Mutation in function "add"
// Only run tests that call "add" (via coverage tracing)
// Skip unrelated tests for "multiplication"
```

**Benefits**:
- **5-10x speed**: If only 20% of tests are relevant
- **Scalability**: Practical for large test suites (1000+ tests)

**Implementation**:
- Integrate with coverage tools (`dotnet-coverage`)
- Build test-to-code mapping (which tests call which functions)
- Filter test execution based on mutation location

### 11. Mutation Diff Viewer
**Current limitation**: Hard to see exact mutations in console output.

**Improvement**: Side-by-side diff viewer:
```
Original Code          |  Mutated Code
-----------------------|----------------------
let add a b = a + b    |  let add a b = a - b
                       |
Status: Killed ✓       |  Operator: OpPlusToMinus
```

**Implementation**:
- CLI: Use `DiffPlex` library for terminal diff
- Web: Generate HTML with `diff2html` style

### 12. Mutation Badges & Metrics
**Current limitation**: No visual representation of mutation score.

**Improvement**: Generate badges for README:
```markdown
![Mutation Score](https://img.shields.io/badge/mutation%20score-85%25-green)
```

**Metrics to track**:
- Mutation score over time (trend)
- Per-module mutation scores
- Mutation operator effectiveness (which operators find most bugs)
- Test execution time per mutation

## References

- **Mutation Testing**: [Wikipedia](https://en.wikipedia.org/wiki/Mutation_testing)
- **FSharp.Compiler.Service**: [Documentation](https://fsharp.github.io/fsharp-compiler-docs/)
- **Similar Tools**: Stryker.NET, PIT (Java), Mutmut (Python)

## License

Educational project for Software Verification course.

## Author

Stevan Dragovic
Uros Dimitrijevic - urosdimit141@gmail.com
Created as part of university coursework on software verification and testing.
