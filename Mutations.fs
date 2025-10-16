namespace FMutant

open System
open FSharp.Compiler.Text

//id generator

type Id = Id of int





// Simple status enum for later runner/reporting.
type MutationStatus =
    | Pending
    | Killed
    | Survived
    | CompileError
    | Timeout
    

// A lightweight record describing a mutation point discovered by the AST walker.
// This is intentionally minimal: it keeps the FCS Range (so the walker need not compute indices),
// a short kind string, and optional token text for convenience.
type Mutation = {
    Id: Id
    FilePath: string
    StartIndex: int
    EndIndex: int
    OriginalText: string
    MutantText: string
    Operator: string
    Status: MutationStatus option
    Notes: string option
}


// A concrete candidate mutation derived from a MutationPoint.
// StartIndex/EndIndex are computed later (using the same SourceText used for parsing)
// so applying text edits is straightforward.
type MutationPoint = {
    FilePath: string
    Range: Range
    NodeKind: string
    TokenText: string option
}



module MutationPoint =
    let create (filePath: string) (nodeKind: string) (range: Range) (tokenText: string option) : MutationPoint =
        {
            FilePath = filePath
            Range = range
            NodeKind = nodeKind
            TokenText = tokenText
        }
    let collect (filePath: string) (nodeKind: string) (range: Range) (tokenText: string option) (acc: MutationPoint list) : MutationPoint list =
        create filePath nodeKind range tokenText :: acc

module Mutation =
    // Convert an FCS Range (1-based lines and 0-based columns in each line) to
    // string start/end indices for a provided ISourceText.
    // Returns (startIndexInclusive, endIndexExclusive)
    // - >The AST (FCS) gives you locations as Range objects (line, column pairs).
    // Those are not absolute string indices — they are (line, column).
    // To extract the exact substring from the file text you must convert that (line, column)
    // range into absolute start/end indices in the file string. That's exactly what rangeToIndices does.

    let rangeToIndices (source: ISourceText) (r:Range) : (int * int) =
        // FCS Range: StartLine/EndLine are 1-based; StartColumn/EndColumn are 0-based
        let startLine = r.StartLine - 1
        let endLine = r.EndLine - 1
        if startLine < 0 || endLine < 0 then
            invalidArg "r" "Range has invalid line numbers"
            
         // Sum lengths of all full lines before startLine, adding single-char newline between lines.
        // This assumes the source uses single-char line endings (LF). This is fine for typical F#
        // source files. If running on CRLF files the column values returned by FCS already
        // account for that when parsing, so using this simpler approach works in practice here.

        let mutable offset = 0
        for i in 0 .. startLine - 1 do
            let line = source.GetLineString i
            offset <- offset + line.Length + 1
            
        let startIndex = offset + r.StartColumn
        
        // compute endIndex by counting until endLine
        // first add remaining of startLine (if multilines) and then intermediate lines
        
        if endLine = startLine then
            let endIndex = startIndex + (r.EndColumn - r.StartColumn)
            (startIndex, endIndex)
        else
            // sum the rest of the start line
            let startLineLen = source.GetLineString startLine |> fun s -> s.Length
            offset <- offset + (startLineLen - r.StartColumn) + 1
            // add full lines between startLine+1 .. endLine-1
            for i in startLine + 1 .. endLine - 1 do
                let line = source.GetLineString i
                offset <- offset + line.Length + 1
            // finally add end line up to EndColumn
            offset <- offset + r.EndColumn
            let endIndex = offset
            (startIndex, endIndex)

    // Safely extract the substring for a given mutation range from source text.
    // -> Once you have absolute indices you can slice the source string to obtain the
    // original source text for that range

    let originalSlice (source: ISourceText) (startIdx: int, endIdx: int) : string =
        // ISourceText doesn't have a direct substring by absolute indices in some versions,
        // but ToString/GetSubText can be used. We'll use ToString() and slice.
        let full = source.ToString()
        if startIdx < 0 || endIdx > full.Length || startIdx > endIdx then
            raise (ArgumentException  $"Invalid slice indices {startIdx}..{endIdx} for source length {full.Length}" )
        full.Substring(startIdx, endIdx - startIdx)

    let private makeMutation  (idGen: unit -> Id) (filePath: string) (startIdx: int) (endIdx: int) (original: string) (mutant: string) (op: string) : Mutation =
        {
            Id = idGen()
            FilePath = filePath
            StartIndex = startIdx
            EndIndex = endIdx
            OriginalText = original
            MutantText = mutant
            Operator = op
            Status = Some Pending
            Notes = None
        }

        
    let createIdGenerator () =
        //TODO: thread safety
        let mutable idInt = 0
        fun () ->
            idInt <- idInt + 1
            idInt |> Id

        
    let createCandidates (idGen: unit -> Id) (source: ISourceText) (point: MutationPoint): Mutation list =
 
        let safeRangeToIndices () =
            try
                Some (rangeToIndices source point.Range)
            with
            | _ -> None
        match safeRangeToIndices() with
        | None -> []
        |Some (startIdx, endIdx) ->
            let original =
                try originalSlice source (startIdx, endIdx)
                with _ -> ""
             // Prefer using TokenText when present and non-empty for operator-based mutations,
            // but fall back to the original slice otherwise.
            let token = match point.TokenText with Some t when t.Trim() <> "" -> Some (t.Trim()) | _ -> None

            // Helpers to construct mutations for simple cases
            let mutationFor replacement opName =
                makeMutation idGen point.FilePath startIdx endIdx original replacement opName
            
            match point.NodeKind with
            | "Const.Bool" ->
                // flip true <-> false
                let origLower = original.Trim().ToLowerInvariant()
                if origLower = "true" then
                    [ mutationFor "false" "BoolFlip"]
                elif origLower = "false" then
                    [ mutationFor "true" "BoolFlip" ]
                else
                    match token with
                    | Some "true" -> [ mutationFor "false" "BoolFlip" ]
                    | Some "false" -> [ mutationFor "true" "BoolFlip" ]
            | "Const.Int32" ->
                // simple numeric mutations: +/- 1
                let trimmed = original.Trim()
                match Int32.TryParse trimmed with
                | (true, v) ->
                    [ mutationFor (string (v + 1)) "IntPlusOne"
                      mutationFor (string (0)) "IntZero"
                      mutationFor (string (-v)) "IntNegative"
                      mutationFor (string (v - 1)) "IntMinusOne" ]
                | _ ->
                    // try token fallback
                    match token with
                    | Some t ->
                        match Int32.TryParse t with
                        | (true, v) ->
                            [ mutationFor (string (v + 1)) "IntPlusOne"
                              mutationFor (string (v - 1)) "IntMinusOne" ]
                        | _ -> []
                    | None -> []
            | "Op.Infix" ->
                let opToken = token |> Option.defaultValue (original.Trim())
                match opToken with
                | "+" -> [ mutationFor "-" "OpPlusToMinus" ]
                | "-" -> [ mutationFor "+" "OpMinusToPlus" ]
                | "*" -> [ mutationFor "/" "OpMulToDiv" ] //Validate for division by zero
                | "/" -> [ mutationFor "*" "OpDivToMul" ]
                | "&&" -> [ mutationFor "||" "OpAndToOr" ]
                | "||" -> [ mutationFor "&&" "OpOrToAnd" ]
                | _ -> []
            | _ -> []

    // Validate that the mutation's OriginalText matches the provided source slice (defensive).
    // Returns true if match; false otherwise.
    let validateOriginalSlice (source: ISourceText) (m: Mutation) : bool =
        try
            let slice = originalSlice source (m.StartIndex, m.EndIndex)
            slice = m.OriginalText
        with
        | _ -> false
