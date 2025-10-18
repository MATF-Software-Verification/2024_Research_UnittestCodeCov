module FMutant.Infrastructure.MutationExecutor

open System.IO
open FMutant.Domain
open FMutant.Infrastructure.FileOperations
open FMutant.Infrastructure.TestRunner


let executeSingleMutation (mutation: Mutation) (timeoutMs: int) : Mutation =
    try
        let originalContent = File.ReadAllText mutation.FilePath
        let mutatedContent = applyMutationToText originalContent mutation
        let backupPath = applyMutationToFile mutation.FilePath mutatedContent

        try
            let compileResult = compileProject ()

            if not (isCompilationSuccess compileResult) then
                { mutation with
                    Status = Some CompileError
                    Notes = Some "Mutation caused compilation error" }
            else
                let testResult = runTests timeoutMs

                if testResult.TimedOut then
                    { mutation with
                        Status = Some Timeout
                        Notes = Some "Test execution timed out" }
                elif not (isTestSuccess testResult) then
                    { mutation with
                        Status = Some Killed
                        Notes = Some "Mutation killed by failing tests" }
                else
                    { mutation with
                        Status = Some Survived
                        Notes = Some "Mutation survived - tests still passed!" }
        finally
            restoreFile mutation.FilePath backupPath
    with
    | ex ->
        { mutation with
            Status = Some CompileError
            Notes = Some $"Error during execution: {ex.Message}" }


let executeMutations (mutations: Mutation list) (timeoutMs: int) : Mutation list =
    mutations
    |> List.mapi (fun index mutation ->
        printfn $"[{index + 1}/{mutations.Length}] Testing mutation {mutation.Id} ({mutation.Operator})..."

        let result = executeSingleMutation mutation timeoutMs

        match result.Status with
        | Some Killed -> printfn "  ✓ Killed"
        | Some Survived -> printfn "  ✗ Survived"
        | Some CompileError -> printfn "  ⚠ Compile Error"
        | Some Timeout -> printfn "  ⏱ Timeout"
        | Some Pending -> printfn "  • Pending"
        | None -> printfn "  ? Unknown"

        result)
