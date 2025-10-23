module FMutant.Program

open System
open System.IO
open FMutant.Domain
open FMutant.Core.AstWalker
open FMutant.Core.MutationOperators
open FMutant.Infrastructure.MutationExecutor
open FMutant.Reporting.Reporter

type MutationTestResult =
    | Success
    | NoMutationsFound
    | InvalidArguments
    | FSharpCoreError of string
    | UnexpectedError of exn

let parseArguments (argv: string array) : Result<string, MutationTestResult> =
    match argv with
    | [| filePath |] -> Ok(filePath)
    | _ -> Error InvalidArguments

let getOutputFunctionList filePath =
    try
        let tree, _ = parseFile filePath
        let points = walkFile tree

        points
        |> List.choose (fun p -> p.FunctionName)
        |> List.distinct
    with
    | ex ->
        eprintfn "Error reading file '%s': %s" filePath ex.Message
        []

let rec chooseOutput (outputs: string list) =
    outputs
    |> List.iteri (fun i o -> printfn "%d) %s" (i + 1) o)

    printf "\nSelect output: "

    match Console.ReadLine() |> Int32.TryParse with
    | true, n when n > 0 && n <= outputs.Length -> outputs.[n - 1]
    | _ ->
        printfn "Invalid selection. Try again.\n"
        chooseOutput outputs

let discoverMutationPoints filePath functionName =
    printfn "Phase 1: Parsing and discovering mutation points..."
    let tree, source = parseFile filePath
    let points = walkFile tree

    let filteredPoints =
        points
        |> List.filter (fun p -> p.FunctionName = Some functionName)

    printfn $"Found {filteredPoints.Length} mutation points in function '{functionName}'\n"
    filteredPoints, source

let generateCandidates (points, source) =
    printfn "Phase 2: Generating mutation candidates..."
    let idGen = createIdGenerator ()

    let candidates =
        points
        |> List.collect (createCandidates idGen source)

    printfn $"Generated {candidates.Length} mutation candidates\n"
    candidates

let executeMutationsAndReport candidates =
    printfn "Phase 3: Executing mutations (this may take a while)..."
    printfn "==========================================\n"
    let timeoutMs = 30000
    let results = executeMutations candidates timeoutMs
    printReport results
    Success

let runMutationTesting filePath functionName =
    try
        printfn $"FMutant - F# Mutation Testing Engine"
        printfn $"Target: {filePath}, Function: {functionName}\n"

        discoverMutationPoints filePath functionName
        |> generateCandidates
        |> function
            | [] ->
                printfn "No mutations to test."
                NoMutationsFound
            | candidates -> executeMutationsAndReport candidates
    with
    | :? System.IO.FileNotFoundException as ex when ex.Message.Contains "FSharp.Core" -> FSharpCoreError ex.Message
    | ex -> UnexpectedError ex

let handleResult =
    function
    | Success -> 0
    | NoMutationsFound -> 0
    | InvalidArguments ->
        printfn "Usage: dotnet run <path-to-fsharp-file> <function-name>"
        0
    | FSharpCoreError msg ->
        printfn $"Error: {msg}"
        printfn "Explanation: A dependency expects a specific FSharp.Core version that is not present at runtime."

        printfn
            "Fix: Align your package versions so FSharp.Compiler.* and FSharp.Core are compatible and restored together."

        1
    | UnexpectedError ex ->
        printfn $"Unhandled error: {ex.ToString()}"
        1

[<EntryPoint>]
let main argv =
    parseArguments argv
    |> Result.map (fun filePath ->
        let outputs = getOutputFunctionList filePath
        let chosen = chooseOutput outputs
        runMutationTesting filePath chosen)
    |> Result.defaultValue InvalidArguments
    |> handleResult
