namespace FMutant.Domain

open FSharp.Compiler.Text

/// Unique identifier for mutations
type Id = Id of int

/// Status of a mutation during testing
type MutationStatus =
    | Pending
    | Killed
    | Survived
    | CompileError
    | Timeout

/// A point in the AST where a mutation can be applied
type MutationPoint = {
    FilePath: string
    Range: Range
    NodeKind: string
    TokenText: string option
    FunctionName: string option
}

/// A concrete mutation candidate with original and mutated text
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

/// Helper functions for creating MutationPoints
module MutationPoint =
    let create
        (filePath: string)
        (nodeKind: string)
        (range: Range)
        (tokenText: string option)
        (functionName: string option)
        : MutationPoint =
        {
            FilePath = filePath
            Range = range
            NodeKind = nodeKind
            TokenText = tokenText
            FunctionName = functionName
        }

    let collect
        (filePath: string)
        (nodeKind: string)
        (range: Range)
        (tokenText: string option)
        (functionName: string option)
        (acc: MutationPoint list)
        : MutationPoint list =
        create filePath nodeKind range tokenText functionName :: acc
