module FMutant.Infrastructure.MutationExecutor

open System.IO
open FMutant.Domain
open FMutant.Infrastructure.FileOperations
open FMutant.Infrastructure.TestRunner

// Lock object for thread-safe console output
let private consoleLock = obj()

// Lock object to ensure complete serialization of file operations
let private fileLock = obj()

let executeSingleMutation (mutation: Mutation) (timeoutMs: int) : Mutation =
    try
        // CRITICAL: Wait to ensure previous file operations are completely done
        // Much longer delay to avoid race conditions on file system
        System.Threading.Thread.Sleep(1000)

        // CRITICAL: Read using SourceText to match how indices were calculated during parsing
        // This ensures indices from rangeToIndices match the content we're mutating
        let sourceText = FSharp.Compiler.Text.SourceText.ofString (File.ReadAllText mutation.FilePath)
        let originalContent = sourceText.ToString()

        let snippet = if originalContent.Length > 300 then originalContent.Substring(260, 40) else originalContent
        printfn $"    DEBUG READ: Content at 260-300: '{snippet}'"

        let mutatedContent = applyMutationToText originalContent mutation
        let backupPath = applyMutationToFile mutation.FilePath mutatedContent

        // CRITICAL: Don't try to delete DLLs - the current process has them locked
        // Instead, rely on dotnet build to detect source changes and rebuild
        // Additional delay to ensure file write is fully flushed to disk
        System.Threading.Thread.Sleep(500)

        try
            let compileResult = compileProject ()

            lock consoleLock (fun () ->
                printfn $"    DEBUG: Mutation {mutation.Id}: '{mutation.OriginalText}' -> '{mutation.MutantText}' at index {mutation.StartIndex}-{mutation.EndIndex}"
                let snippet = if mutatedContent.Length > 300 then mutatedContent.Substring(260, 40) else mutatedContent
                printfn $"    Mutated snippet (260-300): '{snippet}'"
                printfn $"    Compile ExitCode: {compileResult.ExitCode}"
                printfn $"    Compile Output: {compileResult.Output}"
                printfn $"    Compile Error: {compileResult.Error}")

            if not (isCompilationSuccess compileResult) then
                { mutation with
                    Status = Some CompileError
                    Notes = Some $"Mutation caused compilation error: ExitCode={compileResult.ExitCode}, Error={compileResult.Error}" }
            else
                let testResult = runTests timeoutMs

                lock consoleLock (fun () ->
                    printfn $"    Test ExitCode: {testResult.ExitCode}"
                    if testResult.Output.Length > 0 then
                        printfn $"    Test Output (full): {testResult.Output}"
                    if testResult.Error.Length > 0 then
                        printfn $"    Test Error: {testResult.Error}")

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
        // CRITICAL: Lock the entire mutation execution to prevent race conditions
        // This ensures file read -> mutate -> compile -> test -> restore all complete
        // before the next mutation starts
        lock fileLock (fun () ->
            lock consoleLock (fun () ->
                printfn $"[{index + 1}/{mutations.Length}] Testing mutation {mutation.Id} ({mutation.Operator})...")

            let result = executeSingleMutation mutation timeoutMs

            lock consoleLock (fun () ->
                match result.Status with
                | Some Killed -> printfn "  ✓ Killed"
                | Some Survived -> printfn "  ✗ Survived"
                | Some CompileError -> printfn "  ⚠ Compile Error"
                | Some Timeout -> printfn "  ⏱ Timeout"
                | Some Pending -> printfn "  • Pending"
                | None -> printfn "  ? Unknown")

            result))
