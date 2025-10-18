module FMutant.Core.MutationOperators

open System
open FSharp.Compiler.Text
open FMutant.Domain
open FMutant.Core.SourceAnalysis

let createIdGenerator () =
    let mutable idInt = 0

    fun () ->
        idInt <- idInt + 1
        idInt |> Id

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

let private mutateBool
    (mutationFor: string -> string -> Mutation)
    (original: string)
    (token: string option)
    : Mutation list =
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

let private mutateInt
    (mutationFor: string -> string -> Mutation)
    (original: string)
    (token: string option)
    : Mutation list =
    let trimmed = original.Trim()

    match Int32.TryParse trimmed with
    | true, v ->
        [ mutationFor (string (v + 1)) "IntPlusOne"
          mutationFor (string 0) "IntZero"
          mutationFor (string -v) "IntNegative"
          mutationFor (string (v - 1)) "IntMinusOne" ]
    | _ ->
        match token with
        | Some t ->
            match Int32.TryParse t with
            | true, v ->
                [ mutationFor (string (v + 1)) "IntPlusOne"
                  mutationFor (string (v - 1)) "IntMinusOne" ]
            | _ -> []
        | None -> []

let private mutateInfixOp
    (mutationFor: string -> string -> Mutation)
    (original: string)
    (token: string option)
    : Mutation list =
    let opToken = token |> Option.defaultValue (original.Trim())

    match opToken with
    | "+" -> [ mutationFor "-" "OpPlusToMinus" ]
    | "-" -> [ mutationFor "+" "OpMinusToPlus" ]
    | "*" -> [ mutationFor "/" "OpMulToDiv" ]
    | "/" -> [ mutationFor "*" "OpDivToMul" ]
    | "&&" -> [ mutationFor "||" "OpAndToOr" ]
    | "||" -> [ mutationFor "&&" "OpOrToAnd" ]
    | _ -> []

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
        | "Const.Bool" -> mutateBool mutationFor original token
        | "Const.Int32" -> mutateInt mutationFor original token
        | "Op.Infix" -> mutateInfixOp mutationFor original token
        | _ -> []
