module FMutant.Core.AstWalker

open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FMutant.Domain

let parseFile (filepath: string) : ParsedInput * ISourceText =
    let checker = FSharpChecker.Create()
    let sourceText = SourceText.ofString (File.ReadAllText filepath)

    let projOptions, _diagnostics =
        checker.GetProjectOptionsFromScript(filepath, sourceText)
        |> Async.RunSynchronously

    let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions projOptions

    let parseFileResults =
        checker.ParseFile(filepath, sourceText, parsingOptions)
        |> Async.RunSynchronously

    parseFileResults.ParseTree, sourceText

let rec private tryGetOperatorRange (expr: SynExpr) : range option =
    match expr with
    | SynExpr.Ident id -> Some id.idRange
    | SynExpr.LongIdent (_, longDotId, _, _) -> Some longDotId.Range // for qualified operators: List.map, Array.filter, etc.
    | SynExpr.App (_, _, func, _, _) -> tryGetOperatorRange func // for function application like (+) 1 2 where (+)
    | _ -> None

let private getTokenFromSingleLineRange (source: ISourceText) (r: range) : string option =
    if r.StartLine = r.EndLine then
        let lineIdx = r.StartLine - 1 // ISourceText is 0-based
        let line = source.GetLineString lineIdx
        let startCol = r.StartColumn
        let endColExclusive = r.EndColumn

        if startCol >= 0
           && endColExclusive <= line.Length
           && startCol < endColExclusive then
            Some(line.Substring(startCol, endColExclusive - startCol))
        else
            None
    else
        None

let private getFunctionName (pat: SynPat) : string option =
    match pat with
    | SynPat.Named (SynIdent (id, _), _, _, _) -> Some id.idText
    | SynPat.LongIdent (SynLongIdent ([ id ], _, _), _, _, _, _, _) -> Some id.idText
    | _ -> None


let rec walkSynExpr
    (filePath: string)
    (source: ISourceText)
    (functionName: string option)
    (expr: SynExpr)
    : MutationPoint list =
    let mkPoint nodeKind (m: range) tokenText acc =
        MutationPoint.collect filePath nodeKind m tokenText functionName acc

    match expr with
    | SynExpr.App (_, true, func, arg, m) ->
        let opToken =
            tryGetOperatorRange func
            |> Option.bind (getTokenFromSingleLineRange source)
            |> Option.map (fun s -> s.Trim())
            |> Option.filter (fun s -> s <> "")

        let opRange = tryGetOperatorRange func |> Option.defaultValue m
        let here = mkPoint "Op.Infix" opRange opToken []
        let pf = walkSynExpr filePath source functionName func
        let pa = walkSynExpr filePath source functionName arg
        here @ pf @ pa
    | SynExpr.App (_, _, func, arg, _) ->
        let pf = walkSynExpr filePath source functionName func
        let pa = walkSynExpr filePath source functionName arg
        pf @ pa
    | SynExpr.Lambda (_, _, _, body, _, _, _) -> walkSynExpr filePath source functionName body
    | SynExpr.Const (c, m) ->
        match c with
        | SynConst.Bool b -> mkPoint "Const.Bool" m (Some(if b then "true" else "false")) []
        | SynConst.Int32 n -> mkPoint "Const.Int32" m (Some(string n)) []
        | SynConst.Double f -> mkPoint "Const.Double" m (Some(string f)) []
        | SynConst.String (s, _, _) -> mkPoint "Const.String" m (Some s) []
        | _ -> mkPoint "Const.Other" m None []
    | SynExpr.Ident _ -> []
    | _ -> []


// walkFile: ParsedInput -> MutationPoint list
// Sample.fs
//   ↓
// ParsedInput.ImplFile
//   ↓
// ParsedImplFileInput (filePath, modules)
//   ↓
// modules → List of SynModuleOrNamespace
//   ↓
// For each module:
//   decls → [SynModuleDecl.Let, SynModuleDecl.Let, ...]
//   ↓
//   Filter only SynModuleDecl.Let (function definitions)
//     ↓
//     For each Let:
//       bindings → [SynBinding for "add", SynBinding for "isLessThanConst", ...]
//       ↓
//       For each binding:
//         Extract functionName from pattern → "add"
//         Extract expr (function body) → a + b
//         ↓
//         walkSynExpr "Sample.fs" source (Some "add") (a + b)

let walkFile (tree: ParsedInput) : MutationPoint list =
    match tree with
    | ParsedInput.ImplFile implFile -> // BECAUSE F# FILES CAN BE .fs (IMPLEMENTATION) OR .fsi (SIGNATURE)
        let (ParsedImplFileInput (filePath, _, _, _, modules, _, _, _)) = implFile //FROM HERE WE ONLY CARE ABOUT IMPLEMENTATION FILES AND WE DESTRUCTURE TO GET THE MODULES AND FILE PATH
        let source = SourceText.ofString (File.ReadAllText filePath)

        let points = //TRAVERSE THROUGH MODULES
            modules
            |> List.collect (fun (SynModuleOrNamespace (_, _, _, decls, _, _, _, _, _)) ->
                decls //TRAVERSE THROUGH TOP LEVEL DECLARATIONS TO GET TO FUNCTION BINDINGS
                // decls = [
                //     SynModuleDecl.Let (for "add")
                //     SynModuleDecl.Let (for "isLessThanConst")
                //     ...
                // ]

                |> List.collect (function
                    | SynModuleDecl.Let (_, bindings, _) ->
                        bindings //WE ONLY CARE ABOUT LET BINDINGS (FUNCTIONS) HERE WE COULD HAVE TYPES OPEN STATEMENTS ETC.
                        |> List.collect (fun binding ->
                            match binding with
                            | SynBinding (_, _, _, _, _, _, _, _pat, _, expr, _, _, _) ->
                                // SynBinding (
                                //     accessibility,  // public/private
                                //     bindingKind,    // Normal/DoBinding
                                //     mustInline,     // inline keyword?
                                //     isMutable,      // mutable keyword?
                                //     attributes,     // [<Attribute>]
                                //     xmlDoc,         // /// documentation
                                //     valData,        // type info
                                //     _pat,           // ← Pattern: "add", "isLessThanConst", etc.
                                //     returnInfo,     // return type annotation
                                //     expr,           // ← THE FUNCTION BODY (what we walk!)
                                //     ...
                                // )

                                let functionName = getFunctionName _pat
                                walkSynExpr filePath source functionName expr) //WE PROCESS THE FUNCTION BODY EXPRESSION TO FIND MUTATION POINTS
                    | _ -> []))

        points
    | _ ->
        printfn "Not an implementation file."
        []



// walkFile Flow:
// ├─ ParsedInput.ImplFile
// │   └─ modules: [SynModuleOrNamespace]
// │       └─ decls: [SynModuleDecl.Let, SynModuleDecl.Let, ...]
// │           └─ bindings: [SynBinding for each function]
// │               ├─ Extract functionName from pattern
// │               └─ walkSynExpr on function body
// │
// walkSynExpr Flow (Recursive):
// ├─ SynExpr.App (infix) → Create MutationPoint for operator
// │   ├─ Recurse on func
// │   └─ Recurse on arg
// ├─ SynExpr.Const → Create MutationPoint for constant
// ├─ SynExpr.Lambda → Recurse on body
// ├─ SynExpr.Ident → Return []
// └─ _ → Return []
