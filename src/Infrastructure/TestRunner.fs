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
    // Build to a separate output directory to avoid locking conflicts with the running process
    // The current FMutant.exe has bin/Debug loaded, so we build to bin/MutationTest instead
    let result = executeProcess "dotnet" "build --no-incremental --verbosity quiet /p:WarningLevel=0 /p:UseAppHost=false /p:OutputPath=bin/MutationTest/" None

    // Filter out MSB3026/MSB3027 warnings from output to reduce console noise
    let cleanOutput =
        result.Output.Split('\n')
        |> Array.filter (fun line ->
            not (line.Contains("MSB3026") || line.Contains("MSB3027") ||
                 line.Contains("Beginning retry") || line.Contains("Exceeded retry count")))
        |> String.concat "\n"

    { result with Output = cleanOutput }

let runTests (timeoutMs: int) : ProcessResult =
    // Run tests using the DLL from the mutation test output directory
    // We can't use --no-build because the main bin/Debug is locked, so we rebuild to MutationTest
    executeProcess "dotnet" "test --no-build --verbosity quiet /p:OutputPath=bin/MutationTest/" (Some timeoutMs)

let isCompilationSuccess (result: ProcessResult) : bool = result.ExitCode = 0

let isTestSuccess (result: ProcessResult) : bool =
    result.ExitCode = 0 && not result.TimedOut
