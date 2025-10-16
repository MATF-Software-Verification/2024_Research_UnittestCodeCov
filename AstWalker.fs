module FMutant.AstWalker



open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let parseFile (filepath: string) : ParsedInput * ISourceText =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString (File.ReadAllText(filepath))
    let projOptions, _diagnostics =
        checker.GetProjectOptionsFromScript(filepath, sourceText)
        |> Async.RunSynchronously
       
    let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projOptions
    let parseFileResults =
        checker.ParseFile(filepath, sourceText, parsingOptions)
        |> Async.RunSynchronously
    
    
    (parseFileResults.ParseTree, sourceText) 

// Find the operator's range by drilling into the 'func' side of an infix application.
let rec private tryGetOperatorRange (expr: SynExpr) : range option =
    match expr with
    | SynExpr.Ident id -> Some id.idRange
    | SynExpr.LongIdent(_, longDotId, _, _) -> 
        Some longDotId.Range             //->for qualified operators: List.map, Array.filter, etc.
    | SynExpr.App (_, _, func, _, _) ->
        tryGetOperatorRange func        //->for function application like (+) 1 2 where (+)
    | _ -> None

// Extract text for a single-line range from the given source (operators are single-token, single-line).
let private getTokenFromSingleLineRange (source: ISourceText) (r: range) : string option =
    if r.StartLine = r.EndLine then
        let lineIdx = r.StartLine - 1 // ISourceText is 0-based
        let line = source.GetLineString lineIdx
        let startCol = r.StartColumn
        let endColExclusive = r.EndColumn
        if startCol >= 0 && endColExclusive <= line.Length && startCol < endColExclusive then
            Some (line.Substring(startCol, endColExclusive - startCol))
        else None
    else None



let rec walkSynExpr (filePath: string) (source: ISourceText) (expr: SynExpr) : MutationPoint list =
    let mkPoint nodeKind (m: range) tokenText acc =
        MutationPoint.collect filePath nodeKind m tokenText acc

    match expr with
    | SynExpr.App (_, true, func, arg, m) ->
        let opToken =
            tryGetOperatorRange func
            |> Option.bind (getTokenFromSingleLineRange source)
            |> Option.map (fun s -> s.Trim())
            |> Option.filter (fun s -> s <> "")
        let opRange = tryGetOperatorRange func |> Option.defaultValue m
        let here = mkPoint "Op.Infix" opRange opToken []
        let pf = walkSynExpr filePath source func
        let pa = walkSynExpr filePath source arg
        here @ pf @ pa
    | SynExpr.App (_, _, func, arg, _) ->
        let pf = walkSynExpr filePath source func
        let pa = walkSynExpr filePath source arg
        pf @ pa
    | SynExpr.Lambda (_, _, _, body, _, _, _) ->
        walkSynExpr filePath source body
    | SynExpr.Const (c, m) ->
        match c with
        | SynConst.Bool b -> mkPoint "Const.Bool" m (Some (if b then "true" else "false")) []
        | SynConst.Int32 n -> mkPoint "Const.Int32" m (Some (string n)) []
        | SynConst.Double f -> mkPoint "Const.Double" m (Some (string f)) []
        | SynConst.String (s, _, _) -> mkPoint "Const.String" m (Some s) []
        | _ -> mkPoint "Const.Other" m None []
    | SynExpr.Ident _ ->
        []
    | _ ->
        []

let walkFile (tree: ParsedInput) : MutationPoint list =
    match tree with
    | ParsedInput.ImplFile implFile ->
        let (ParsedImplFileInput(filePath, _, _, _, modules, _, _, _)) = implFile
        // Load source once for this file so we can slice tokens by ranges.
        let source = SourceText.ofString (File.ReadAllText filePath)
        let points =
            modules
            |> List.collect (fun (SynModuleOrNamespace(_, _, _, decls, _, _, _, _, _)) ->
                decls
                |> List.collect (function
                    | SynModuleDecl.Let (_, bindings, _) ->
                        bindings
                        |> List.collect (fun binding ->
                            match binding with
                            | SynBinding(_, _, _, _, _, _, _, _pat, _, expr, _, _, _) ->
                                walkSynExpr filePath source expr)
                    | _ -> []))
        points
    | _ -> 
        printfn "Not an implementation file."
        []

