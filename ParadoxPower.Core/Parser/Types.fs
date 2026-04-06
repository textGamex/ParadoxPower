namespace ParadoxPower.Parser

open ParadoxPower.Process
open ParadoxPower.Utilities.Position
open FParsec
open System.Globalization

[<AutoOpen>]
module Types =
#if NET5_0_OR_GREATER
    [<System.Runtime.CompilerServices.IsReadOnly>]
#endif
    [<Struct>]
    type Position =
        | Position of FParsec.Position

        override x.ToString() =
            let (Position p) = x in $"Position (Ln: %i{p.Line}, Pos: %i{p.Column}, File: %s{p.StreamName})"

        static member Empty = Position(FParsec.Position("none", 0L, 0L, 0L))

        static member File(fileName) =
            Position(FParsec.Position(fileName, 0L, 0L, 0L))

        static member Conv(pos: FParsec.Position) = Position pos
        static member UnConv(pos: Position) = let (Position p) = pos in p

    type Operator =
        | Equals = 0uy
        | GreaterThan = 1uy
        | LessThan = 2uy
        | GreaterThanOrEqual = 3uy
        | LessThanOrEqual = 4uy
        | NotEqual = 5uy
        | EqualEqual = 6uy
        | QuestionEqual = 7uy

    let operatorToString =
        function
        | Operator.Equals -> "="
        | Operator.GreaterThan -> ">"
        | Operator.LessThan -> "<"
        | Operator.GreaterThanOrEqual -> ">="
        | Operator.LessThanOrEqual -> "<="
        | Operator.NotEqual -> "!="
        | Operator.EqualEqual -> "=="
        | Operator.QuestionEqual -> "?="
        | x -> failwith (sprintf "Unknown enum value %A" x)

#if NET5_0_OR_GREATER
    [<System.Runtime.CompilerServices.IsReadOnly>]
#endif
    [<Struct>]
    type Key =
        | Key of string

        override x.ToString() = let (Key v) = x in v

#if NET5_0_OR_GREATER
    [<System.Runtime.CompilerServices.IsReadOnly>]
#endif
    [<Struct>]
    type KeyValueItem =
        | KeyValueItem of Key * Value * Operator

        override x.ToString() =
            let (KeyValueItem(id, v, op)) = x in $"{id} %s{operatorToString op} {v}"

    and Value =
        | String of string
        | QString of string
        | Float of decimal
        | Int of int
        | Bool of bool
        | Clause of Statement list

        override x.ToString() =
            match x with
            | Clause b -> $"{{ {b} }}"
            | QString s -> "\"" + s.Replace("\"", "\\\"") + "\""
            | String s -> s
            | Bool b -> if b then "yes" else "no"
            | Float f -> f.ToString(CultureInfo.InvariantCulture)
            | Int i -> $"%i{i}"

        member x.ToRawString() =
            match x with
            | Clause b -> $"{{ {b} }}"
            | QString s -> s
            | String s -> s
            | Bool b -> if b then "yes" else "no"
            | Float f -> f.ToString(CultureInfo.InvariantCulture)
            | Int i -> $"%i{i}"

    and [<CustomEquality; NoComparison; Struct>] PosKeyValue =
        | PosKeyValue of Range * KeyValueItem

        override x.Equals(y) =
            match y with
            | :? PosKeyValue as y ->
                let (PosKeyValue(_, k)) = x
                let (PosKeyValue(_, k2)) = y
                k = k2
            | _ -> false

        override x.GetHashCode() =
            let (PosKeyValue(_, k)) = x in k.GetHashCode()

    and [<CustomEquality; NoComparison>] Statement =
        | CommentStatement of Comment
        | KeyValue of PosKeyValue
        | Value of Range * Value

        override x.Equals(y) =
            match y with
            | :? Statement as y ->
                match x, y with
                | CommentStatement({Position=r1; Comment=s1}), CommentStatement({Position=r2; Comment=s2}) -> s1 = s2 && r1 = r2
                | KeyValue kv1, KeyValue kv2 -> kv1 = kv2
                | Value(r1, v1), Value(r2, v2) -> r1 = r2 && v1 = v2
                | _ -> false
            | _ -> false

        override x.GetHashCode() =
            match x with
            | CommentStatement({Position=r; Comment=c}) -> hash (r, c)
            | KeyValue kv -> kv.GetHashCode()
            | Value(r, v) -> hash (r, v)

    [<StructuralEquality; NoComparison>]
    type ParsedFile = ParsedFile of Statement list

    type ParseFile = string -> ParserResult<ParsedFile, unit>
    type ParseString = string -> string -> ParserResult<ParsedFile, unit>
    type PrettyPrintFile = ParsedFile -> string
    type PrettyPrintStatements = Statement list -> string
    type PrettyPrintStatement = Statement -> string
    type PrettyPrintFileResult = ParserResult<ParsedFile, unit> -> string

    type ParserAPI =
        { parseFile: ParseFile
          parseString: ParseString }

    type PrinterAPI =
        { PrettyPrintFile: PrettyPrintFile
          PrettyPrintStatements: PrettyPrintStatements
          PrettyPrintStatement: PrettyPrintStatement
          PrettyPrintFileResult: PrettyPrintFileResult }
