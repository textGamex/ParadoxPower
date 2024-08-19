namespace ParadoxPower.CSharp

open ParadoxPower.Parser
open ParadoxPower.Process.ProcessCore
open ParadoxPower.Utilities.Utils

[<NoEquality; NoComparison>]
type ParserError =
    { Filename: string
      Line: int64
      Column: int64
      ErrorMessage: string }

type Parsers =
    static member ParseScriptFile(filename: string, filetext: string) = CKParser.parseString filetext filename

    static member ProcessStatements(filename: string, filepath: string, statements: Statement list) =
        simpleProcess.ProcessNode () filename (mkZeroFile filepath) statements
