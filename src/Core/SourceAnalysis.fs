module FMutant.Core.SourceAnalysis

open System
open FSharp.Compiler.Text
open FMutant.Domain

let rangeToIndices (source: ISourceText) (r: Range) : int * int =
    let startLine = r.StartLine - 1
    let endLine = r.EndLine - 1

    if startLine < 0 || endLine < 0 then
        invalidArg "r" "Range has invalid line numbers"

    let mutable offset = 0

    for i in 0 .. startLine - 1 do
        let line = source.GetLineString i
        offset <- offset + line.Length + 1

    let startIndex = offset + r.StartColumn

    if endLine = startLine then
        let endIndex = startIndex + (r.EndColumn - r.StartColumn)
        startIndex, endIndex
    else
        let startLineLen =
            source.GetLineString startLine
            |> fun s -> s.Length

        offset <- offset + (startLineLen - r.StartColumn) + 1

        for i in startLine + 1 .. endLine - 1 do
            let line = source.GetLineString i
            offset <- offset + line.Length + 1

        offset <- offset + r.EndColumn
        let endIndex = offset
        startIndex, endIndex

let originalSlice (source: ISourceText) (startIdx: int, endIdx: int) : string =
    let full = source.ToString()

    if startIdx < 0
       || endIdx > full.Length
       || startIdx > endIdx then
        raise (ArgumentException $"Invalid slice indices {startIdx}..{endIdx} for source length {full.Length}")

    full.Substring(startIdx, endIdx - startIdx)

let validateOriginalSlice (source: ISourceText) (m: Mutation) : bool =
    try
        let slice = originalSlice source (m.StartIndex, m.EndIndex)
        slice = m.OriginalText
    with
    | _ -> false
