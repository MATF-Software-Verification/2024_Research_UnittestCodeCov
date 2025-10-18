module FMutant.Reporting.Reporter

open FMutant.Domain


type MutationStats =
    { Total: int
      Killed: int
      Survived: int
      CompileErrors: int
      Timeouts: int
      MutationScore: float }

let calculateStats (results: Mutation list) : MutationStats =
    let total = results.Length

    let killed =
        results
        |> List.filter (fun m -> m.Status = Some Killed)
        |> List.length

    let survived =
        results
        |> List.filter (fun m -> m.Status = Some Survived)
        |> List.length

    let compileErrors =
        results
        |> List.filter (fun m -> m.Status = Some CompileError)
        |> List.length

    let timeouts =
        results
        |> List.filter (fun m -> m.Status = Some Timeout)
        |> List.length

    let validMutations = killed + survived

    let mutationScore =
        if validMutations > 0 then
            (float killed / float validMutations) * 100.0
        else
            0.0

    { Total = total
      Killed = killed
      Survived = survived
      CompileErrors = compileErrors
      Timeouts = timeouts
      MutationScore = mutationScore }

let printReport (results: Mutation list) : unit =
    printfn "\n=========================================="
    printfn "MUTATION TESTING REPORT"
    printfn "==========================================\n"

    let stats = calculateStats results

    printfn $"Total Mutations:     {stats.Total}"
    printfn $"Killed:              {stats.Killed}"
    printfn $"Survived:            {stats.Survived}"
    printfn $"Compile Errors:      {stats.CompileErrors}"
    printfn $"Timeouts:            {stats.Timeouts}"
    printfn $"\nMutation Score:      {stats.MutationScore:F2}%%"
    printfn "=========================================="

    if stats.Survived > 0 then
        printfn "\nSURVIVING MUTATIONS (Weak Test Coverage):"
        printfn "------------------------------------------"

        results
        |> List.filter (fun m -> m.Status = Some Survived)
        |> List.iter (fun m ->
            printfn $"  [{m.Id}] {m.Operator}: \"{m.OriginalText}\" → \"{m.MutantText}\""
            printfn $"  Location: {m.FilePath}:{m.StartIndex}")

        printfn ""


let printSummary (stats: MutationStats) : unit =
    printfn $"Mutation Score: {stats.MutationScore:F2}%% ({stats.Killed}/{stats.Killed + stats.Survived} killed)"
