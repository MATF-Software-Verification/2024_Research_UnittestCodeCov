module FMutant.Infrastructure.TestRunner

open System.Diagnostics
open System.Text

type ProcessResult =
    { ExitCode: int
      Output: string
      Error: string
      TimedOut: bool }

let private executeProcess (fileName: string) (arguments: string) (timeoutMs: int option) : ProcessResult =
    let psi = ProcessStartInfo()
    psi.FileName <- fileName
    psi.Arguments <- arguments
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    use proc = new Process()
    proc.StartInfo <- psi

    let output = StringBuilder()
    let error = StringBuilder()

    proc.OutputDataReceived.Add (fun args ->
        if not (isNull args.Data) then
            output.AppendLine args.Data |> ignore)

    proc.ErrorDataReceived.Add (fun args ->
        if not (isNull args.Data) then
            error.AppendLine args.Data |> ignore)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()

    let completed =
        match timeoutMs with
        | Some timeout -> proc.WaitForExit timeout
        | None ->
            proc.WaitForExit()
            true

    if not completed then
        try
            proc.Kill()
        with
        | _ -> ()

        { ExitCode = -1
          Output = output.ToString()
          Error = "Process timed out"
          TimedOut = true }
    else
        { ExitCode = proc.ExitCode
          Output = output.ToString()
          Error = error.ToString()
          TimedOut = false }

let compileProject () : ProcessResult =
    executeProcess "dotnet" "build --no-restore --verbosity quiet" None

let runTests (timeoutMs: int) : ProcessResult =
    executeProcess "dotnet" "test --no-build --verbosity quiet" (Some timeoutMs)

let isCompilationSuccess (result: ProcessResult) : bool = result.ExitCode = 0

let isTestSuccess (result: ProcessResult) : bool =
    result.ExitCode = 0 && not result.TimedOut
