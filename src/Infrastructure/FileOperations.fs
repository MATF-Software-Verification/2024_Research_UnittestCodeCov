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
        File.Copy(backupPath, filePath, overwrite = true)
        File.Delete backupPath
    | false -> ()


let applyMutationToFile (filePath: string) (mutatedContent: string) : string =
    let backupPath = backupFile filePath
    File.WriteAllText(filePath, mutatedContent)
    backupPath


let cleanupBackup (backupPath: string) : unit =
    backupPath
    |> File.Exists
    |> function
        | true -> File.Delete backupPath
        | false -> ()
