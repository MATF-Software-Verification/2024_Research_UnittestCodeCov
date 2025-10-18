namespace FMutant

open System
open FSharp.Compiler.Text


type Id = Id of int





type MutationStatus =
    | Pending
    | Killed
    | Survived
    | CompileError
    | Timeout



type MutationPoint =
    { FilePath: string
      Range: Range
      NodeKind: string
      TokenText: string option
      FunctionName: string option }


type Mutation =
    { Id: Id
      FilePath: string
      StartIndex: int
      EndIndex: int
      OriginalText: string
      MutantText: string
      Operator: string
      Status: MutationStatus option
      Notes: string option }



module MutationPoint =
    let create
        (filePath: string)
        (nodeKind: string)
        (range: Range)
        (tokenText: string option)
        (functionName: string option)
        : MutationPoint =
        { FilePath = filePath
          Range = range
          NodeKind = nodeKind
          TokenText = tokenText
          FunctionName = functionName }

    let collect
        (filePath: string)
        (nodeKind: string)
        (range: Range)
        (tokenText: string option)
        (functionName: string option)
        (acc: MutationPoint list)
        : MutationPoint list =
        create filePath nodeKind range tokenText functionName
        :: acc

module Mutation =

    let rangeToIndices (source: ISourceText) (r: Range) : (int * int) =

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
            (startIndex, endIndex)
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
            (startIndex, endIndex)


    let originalSlice (source: ISourceText) (startIdx: int, endIdx: int) : string =

        let full = source.ToString()

        if startIdx < 0
           || endIdx > full.Length
           || startIdx > endIdx then
            raise (ArgumentException $"Invalid slice indices {startIdx}..{endIdx} for source length {full.Length}")

        full.Substring(startIdx, endIdx - startIdx)

    let private makeMutation
        (idGen: unit -> Id)
        (filePath: string)
        (startIdx: int)
        (endIdx: int)
        (original: string)
        (mutant: string)
        (op: string)
        : Mutation =
        { Id = idGen ()
          FilePath = filePath
          StartIndex = startIdx
          EndIndex = endIdx
          OriginalText = original
          MutantText = mutant
          Operator = op
          Status = Some Pending
          Notes = None }


    let createIdGenerator () =
        let mutable idInt = 0

        fun () ->
            idInt <- idInt + 1
            idInt |> Id


    let createCandidates (idGen: unit -> Id) (source: ISourceText) (point: MutationPoint) : Mutation list =

        let safeRangeToIndices () =
            try
                Some(rangeToIndices source point.Range)
            with
            | _ -> None

        match safeRangeToIndices () with
        | None -> []
        | Some (startIdx, endIdx) ->
            let original =
                try
                    originalSlice source (startIdx, endIdx)
                with
                | _ -> ""

            let token =
                match point.TokenText with
                | Some t when t.Trim() <> "" -> Some(t.Trim())
                | _ -> None


            let mutationFor replacement opName =
                makeMutation idGen point.FilePath startIdx endIdx original replacement opName

            match point.NodeKind with
            | "Const.Bool" ->
                let origLower = original.Trim().ToLowerInvariant()

                if origLower = "true" then
                    [ mutationFor "false" "BoolFlip" ]
                elif origLower = "false" then
                    [ mutationFor "true" "BoolFlip" ]
                else
                    match token with
                    | Some "true" -> [ mutationFor "false" "BoolFlip" ]
                    | Some "false" -> [ mutationFor "true" "BoolFlip" ]
                    | _ -> []
            | "Const.Int32" ->

                let trimmed = original.Trim()

                match Int32.TryParse trimmed with
                | (true, v) ->
                    [ mutationFor (string (v + 1)) "IntPlusOne"
                      mutationFor (string (0)) "IntZero"
                      mutationFor (string (-v)) "IntNegative"
                      mutationFor (string (v - 1)) "IntMinusOne" ]
                | _ ->
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


    let validateOriginalSlice (source: ISourceText) (m: Mutation) : bool =
        try
            let slice = originalSlice source (m.StartIndex, m.EndIndex)
            slice = m.OriginalText
        with
        | _ -> false
