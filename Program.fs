module FMutant.Program

open FMutant.AstWalker


[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Usage: dotnet run <path-to-fsharp-file>"
        0
    else
        let filePath = argv[0]
        try
            let tree, source = parseFile filePath
            let points = walkFile tree
            
            let idGen = Mutation.createIdGenerator()
            let candidates =
                points
                |> List.collect (Mutation.createCandidates idGen source)

            points
            |> List.iter (fun p ->
                let s = p.Range.Start
                let e = p.Range.End
                let token = p.TokenText |> Option.defaultValue ""
                printfn "MutationPoint: %s [%d:%d - %d:%d] token=\"%s\" file=%s"
                        p.NodeKind s.Line s.Column e.Line e.Column token p.FilePath)
            candidates
            |> List.iter (fun m ->
                printfn "Mutation: id=%A file=%s range=%d..%d op=%s original=\"%s\" mutant=\"%s\" status=%A notes=%A"
                    m.Id m.FilePath m.StartIndex m.EndIndex m.Operator m.OriginalText m.MutantText m.Status m.Notes)

            0

        with
        | :? System.IO.FileNotFoundException as ex when ex.Message.Contains("FSharp.Core") ->
            printfn $"Error: {ex.Message}" 
            printfn "Explanation: A dependency expects a specific FSharp.Core version that is not present at runtime."
            printfn "Fix: Align your package versions so FSharp.Compiler.* and FSharp.Core are compatible and restored together."
            1
        | ex ->
            printfn $"Unhandled error: {ex.ToString()}" 
            1