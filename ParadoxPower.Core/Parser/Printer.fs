namespace ParadoxPower.Parser

open Types
open FParsec

module Printer =
    open System
    let private tabs n = new string ('\t', n)

    let rec private printValue v depth =
        match v with
        | Clause kvl -> "{" + Environment.NewLine + printKeyValueList kvl (depth + 1) + tabs depth + "}"
        | x -> x.ToString()

    and private printKeyValue (acc, leadingNewline, prevStart, prevEnd) kv depth =
        match kv with
        | CommentStatement({ Position = r; Comment = c }) ->
            if r.StartLine = prevStart && r.StartLine = prevEnd || (not leadingNewline) then
                acc + (tabs depth) + "#" + c, true, r.StartLine, r.EndLine
            else
                acc + Environment.NewLine + (tabs depth) + "#" + c, true, r.StartLine, r.EndLine
        | KeyValue(PosKeyValue(r, KeyValueItem(key, v, op))) ->
            acc
            + (if leadingNewline then Environment.NewLine else "")
            + (tabs depth)
            + key.ToString()
            + " "
            + operatorToString op
            + " "
            + (printValue v depth),
            true,
            r.StartLine,
            r.EndLine
        | Value(r, v) ->
            acc
            + (if leadingNewline then Environment.NewLine else "")
            + (tabs depth)
            + (printValue v depth),
            true,
            r.StartLine,
            r.EndLine

    and private printKeyValueList (kvl: Statement seq) (depth: int) : string =
        kvl
        |> Seq.fold (fun acc kv -> printKeyValue acc kv depth) ("", false, -1, -1)
        |> (fun (res, leadingNewline, _, _) -> if leadingNewline then res + Environment.NewLine else res)

    let printTopLevelKeyValueList kvl =
        kvl
        |> List.fold
            (fun acc kv ->
                match kv with
                | KeyValue(PosKeyValue(_, KeyValueItem(_, Clause _, _))) as x ->
                    let res, a, b, c = printKeyValue acc kv 0
                    (res + Environment.NewLine, a, b, c)
                | x -> printKeyValue acc kv 0)
            ("", false, -1, -1)
        |> (fun (res, _, _, _) -> res)

    let private prettyPrint ef =
        let (ParsedFile sl) = ef
        printKeyValueList sl 0

    let private prettyPrintResult =
        function
        | Success(v, _, _) ->
            let (ParsedFile ev) = v
            printKeyValueList ev 0
        | Failure(msg, _, _) -> msg

    let PrettyPrintStatements (statements: Statement seq) = printKeyValueList statements 0

    let PrettyPrintFile file = prettyPrint file

    let PrettyPrintStatement statement = printKeyValueList [| statement |] 0

    let PrettyPrintFileResult = prettyPrintResult
