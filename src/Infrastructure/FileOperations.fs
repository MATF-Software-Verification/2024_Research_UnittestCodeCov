module FMutant.Infrastructure.FileOperations

open System.IO
open FMutant.Domain


let applyMutationToText (sourceText: string) (mutation: Mutation) : string =
    let before = sourceText.Substring(0, mutation.StartIndex)
    let after = sourceText.Substring(mutation.EndIndex)
    before + mutation.MutantText + after


let backupFile (filePath: string) : string =
    let backupPath = filePath + ".backup"

    filePath
    |> File.Exists
    |> function
        | true -> File.Copy(filePath, backupPath, overwrite = true)
        | false -> ()

    backupPath


let restoreFile (filePath: string) (backupPath: string) : unit =
    match File.Exists backupPath with
    | true ->
        // Read backup content completely into memory first
        let backupContent = File.ReadAllText(backupPath)

        // Write it back to the original file location
        File.WriteAllText(filePath, backupContent)

        // Force synchronization - ensure write is committed to disk
        use fileStream = new System.IO.FileStream(filePath, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.None)
        fileStream.Flush(true)
        // use statement will auto-close and dispose

        // CRITICAL: Much longer wait to ensure file system commits
        System.Threading.Thread.Sleep(500)

        // Delete backup
        File.Delete backupPath

        // Additional delay to ensure complete file system consistency
        System.Threading.Thread.Sleep(200)
    | false -> ()


let applyMutationToFile (filePath: string) (mutatedContent: string) : string =
    let backupPath = backupFile filePath

    printfn $"    DEBUG WRITE: Writing to {filePath}, content length: {mutatedContent.Length}"

    // Write mutated content to file
    File.WriteAllText(filePath, mutatedContent)

    // Verify it was written
    let verifyContent = File.ReadAllText(filePath)
    printfn $"    DEBUG VERIFY: Read back length: {verifyContent.Length}, matches: {verifyContent = mutatedContent}"

    // Force synchronization - ensure write is committed to disk
    use fileStream = new System.IO.FileStream(filePath, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.None)
    fileStream.Flush(true)
    // use statement will auto-close and dispose

    // Wait for file system to fully commit
    System.Threading.Thread.Sleep(200)

    backupPath


let cleanupBackup (backupPath: string) : unit =
    backupPath
    |> File.Exists
    |> function
        | true -> File.Delete backupPath
        | false -> ()
