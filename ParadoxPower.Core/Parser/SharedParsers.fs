﻿namespace ParadoxPower.Parser

open ParadoxPower.Process
open ParadoxPower.Utilities
open FParsec
open ParadoxPower.Utilities.Position
open Types
open ParadoxPower.Utilities.Utils

module internal SharedParsers =
    let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
        fun stream ->
            log $"%A{stream.Position}: Entering %s{label}"
            let reply = p stream
            log $"%A{stream.Position}: Leaving %s{label} (%A{reply.Status})"
            reply

    let betweenL (popen: Parser<_, _>) (pclose: Parser<_, _>) (p: Parser<_, _>) label =
        let notClosedError (pos: FParsec.Position) =
            messageError $"The %s{label} opened at %s{pos.ToString()} was not closed."

        let expectedLabel = expected label

        fun (stream: CharStream<_>) ->
            // The following code might look a bit complicated, but that's mainly
            // because we manually apply three parsers in sequence and have to merge
            // the errors when they refer to the same parser state.
            let state0 = stream.State
            let reply1 = popen stream

            if reply1.Status = Ok then
                let stateTag1 = stream.StateTag
                let reply2 = p stream

                let error2 =
                    if stateTag1 <> stream.StateTag then
                        reply2.Error
                    else
                        mergeErrors reply1.Error reply2.Error

                if reply2.Status = Ok then
                    let stateTag2 = stream.StateTag
                    let reply3 = pclose stream

                    let error3 =
                        if stateTag2 <> stream.StateTag then
                            reply3.Error
                        else
                            mergeErrors error2 reply3.Error

                    if reply3.Status = Ok then
                        Reply(Ok, reply2.Result, error3)
                    else
                        Reply(reply3.Status, mergeErrors error3 (notClosedError (state0.GetPosition(stream))))
                else
                    Reply(reply2.Status, reply2.Error)
            else
                let error =
                    if state0.Tag <> stream.StateTag then
                        reply1.Error
                    else
                        expectedLabel

                Reply(reply1.Status, error)

    // Sets of chars
    // =======

    let idCharArray =
        [| '_'
           ':'
           '@'
           '.'
           '\"'
           '-'
           '''
           '['
           ']'
           '!'
           '<'
           '>'
           '$'
           '^'
           '&'
           '|'
           magicChar |]

    let isAnyofidCharArray = isAnyOf idCharArray
    let isidchar = fun c -> isLetter c || isDigit c || isAnyofidCharArray c

    let valueCharArray =
        [| '_'
           '.'
           '-'
           ':'
           ';'
           '\''
           '['
           ']'
           '@'
           '''
           '+'
           '`'
           '%'
           '/'
           '!'
           ','
           '<'
           '>'
           '?'
           '$'
           'š'
           'Š'
           '’'
           '|'
           '^'
           '*'
           '&'
           magicChar |]

    let isAnyValueChar = isAnyOf valueCharArray
    let isvaluechar = fun c -> isLetter c || isDigit c || isAnyValueChar c


    // Utility parsers
    // =======
    let ws = spaces <?> "whitespace"
    let str s = pstring s .>> ws <?> ("string " + s)

    let strSkip s =
        skipString s .>> ws <?> ("skip string " + s)

    let ch c = pchar c .>> ws <?> ("char " + string c)

    let chSkip c =
        skipChar c .>> ws <?> ("skip char " + string c)
    // let clause inner = between (chSkip '{') (skipChar '}') inner
    let clause inner =
        betweenL (chSkip '{' <?> "opening brace") (skipChar '}' <?> "closing brace") inner "clause"

    let quotedCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = (pstring "\\\"" <|> pstring "\\") |>> string
    let metaprogrammingCharSnippet = many1Satisfy (fun c -> c <> ']' && c <> '\\')

    let getRange (start: FParsec.Position) (endp: FParsec.Position) =
        mkRange
            start.StreamName
            (mkPos (int start.Line) (int start.Column - 1))
            (mkPos (int endp.Line) (int endp.Column - 1))

    let parseWithPosition p =
        pipe3 getPosition p getPosition (fun s r e -> getRange s e, r)

    // Base types
    // =======
    let oppLTE = skipString "<=" |>> (fun _ -> Operator.LessThanOrEqual)
    let oppGTE = skipString ">=" |>> (fun _ -> Operator.GreaterThanOrEqual)
    let oppNE = skipString "!=" |>> (fun _ -> Operator.NotEqual)
    let oppEE = skipString "==" |>> (fun _ -> Operator.EqualEqual)
    let oppQE = skipString "?=" |>> (fun _ -> Operator.QuestionEqual)
    let oppLT = skipChar '<' |>> (fun _ -> Operator.LessThan)
    let oppGT = skipChar '>' |>> (fun _ -> Operator.GreaterThan)
    let oppE = skipChar '=' |>> (fun _ -> Operator.Equals)

    let operator =
        choiceL [ oppLTE; oppGTE; oppNE; oppEE; oppLT; oppGT; oppE; oppQE ] "operator"
        .>> ws

    let operatorLookahead =
        choice [ chSkip '='; chSkip '>'; chSkip '<'; chSkip '!'; strSkip "?=" ]
        <?> "operator 1"

    let comment =
        parseWithPosition (skipChar '#' >>. restOfLine true .>> ws |>> string)
        <?> "comment"

    let key = (many1SatisfyL isidchar "id character") .>> ws |>> Key <?> "id"

    let keyQ =
        between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar))
        .>> ws
        |>> (fun s -> "\"" + s + "\"")
        |>> Key
        <?> "quoted key"

    let valueS =
        (many1SatisfyL isvaluechar "value character")
        |>> string
        |>> (fun x -> StringResource.stringManager.InternIdentifierToken x)
        |>> String
        <?> "string"

    // let valueQ = between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar)) |>> QString <?> "quoted string"
    let valueQ =
        betweenL (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar)) "quoted string"
        |>> (fun x -> StringResource.stringManager.InternIdentifierToken x)
        |>> QString
        <?> "quoted string"

    // let valueB = ( (skipString "yes") .>> nextCharSatisfiesNot (isvaluechar)  |>> (fun _ -> Bool(true))) <|>
    //                 ((skipString "no") .>> nextCharSatisfiesNot (isvaluechar)  |>> (fun _ -> Bool(false)))
    let valueBYes =
        skipString "yes" .>> nextCharSatisfiesNot isvaluechar |>> (fun _ -> Bool(true))

    let valueBNo =
        skipString "no" .>> nextCharSatisfiesNot isvaluechar |>> (fun _ -> Bool(false))

    let valueI = pint64 .>> nextCharSatisfiesNot isvaluechar |>> int |>> Int
    let valueF = pfloat .>> nextCharSatisfiesNot isvaluechar |>> decimal |>> Float

    let hsv3 =
        clause (
            pipe3
                ((parseWithPosition valueF .>> ws) .>> ws)
                (parseWithPosition valueF .>> ws)
                (parseWithPosition valueF .>> ws)
                (fun a b c -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    let hsv4 =
        clause (
            pipe4
                (parseWithPosition valueF .>> ws)
                (parseWithPosition valueF .>> ws)
                (parseWithPosition valueF .>> ws)
                (parseWithPosition valueF .>> ws)
                (fun a b c d -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ])
        )

    let hsvI =
        clause (
            pipe4
                (parseWithPosition valueF .>> ws)
                (parseWithPosition valueF .>> ws)
                (parseWithPosition valueF .>> ws)
                (opt (parseWithPosition valueF .>> ws))
                (fun a b c d ->
                    match (a, b, c, d) with
                    | a, b, c, Some d ->
                        Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ]
                    | a, b, c, None -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    let hsv = strSkip "hsv" >>. opt (strSkip "360") >>. hsvI .>> ws
    let hsvC = strSkip "HSV" >>. hsvI .>> ws

    let rgbI =
        clause (
            pipe4
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (opt (parseWithPosition valueI .>> ws))
                (fun a b c d ->
                    match (a, b, c, d) with
                    | a, b, c, Some d ->
                        Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ]
                    | a, b, c, None -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )


    let rgb3 =
        clause (
            pipe3
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (fun a b c -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    let rgb4 =
        clause (
            pipe4
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (parseWithPosition valueI .>> ws)
                (fun a b c d -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ])
        )

    let rgb = strSkip "rgb" >>. rgbI .>> ws
    let rgbC = strSkip "RGB" >>. rgbI .>> ws

    let metaprograming =
        pipe3 (pstring "@\\[") metaprogrammingCharSnippet (ch ']') (fun a b c -> (a + b + string c))
        |>> (fun x -> StringResource.stringManager.InternIdentifierToken x)
        |>> String
    // Complex types
    // =======

    // Recursive types
    let keyvalue, keyvalueimpl = createParserForwardedToRef ()
    let (value: Parser<Value, unit>), valueimpl = createParserForwardedToRef ()

    let leafValue =
        pipe3 getPosition (value .>> ws) getPosition (fun a b c -> (getRange a c, b)) // |>> (fun (p, v) -> p, (Value v)))

    let statement =
        comment |>> (fun (range, str) -> CommentStatement({Position=range; Comment=str}))
        <|> (attempt (leafValue .>> notFollowedBy operatorLookahead |>> Value))
        <|> keyvalue
        <?> "statement"

    let valueBlock =
        clause (many1 ((leafValue |>> Value) <|> (comment |>> (fun (range, str) -> CommentStatement({Position=range; Comment=str}))))) |>> Clause
        <?> "value clause"

    let valueClause = clause (many statement) |>> Clause //<?> "statement clause"

    let valueCustom: Parser<Value, unit> =
        let vcP = valueClause
        let iP = attempt valueI
        let fP = attempt valueF
        let byP = attempt valueBYes <|> valueS
        let bnP = attempt valueBNo <|> valueS
        let mpP = metaprograming

        fun (stream: CharStream<_>) ->
            match stream.Peek() with
            | '{' -> vcP stream
            | '"' -> valueQ stream
            | x when isDigit x || x = '-' ->
                let i = (iP stream)

                if i.Status = Ok then
                    i
                else
                    let f = (fP stream)
                    if f.Status = Ok then f else valueS stream
            | _ ->
                match stream.PeekString 3, stream.PeekString 2 with
                | "rgb", _ -> rgb stream
                | "RGB", _ -> rgbC stream
                | "hsv", _ -> hsv stream
                | "HSV", _ -> hsvC stream
                | "yes", _ -> byP stream
                | _, "no" -> bnP stream
                | "@\\[", _ -> mpP stream
                | _ -> valueS stream
    //| _ -> choice [(attempt valueB); valueS] stream
    //choiceL [(attempt valueB); (attempt hsv); (attempt rgb); valueS] "value" stream

    valueimpl.Value <- valueCustom <?> "value"

    keyvalueimpl.Value
        <- pipe5 getPosition (keyQ <|> key) operator value (getPosition .>> ws) (fun start id op value endp ->
        KeyValue(PosKeyValue(getRange start endp, KeyValueItem(id, value, op))))

    let alle = ws >>. many statement .>> eof |>> (fun f -> (ParsedFile f))

    let valuelist =
        many1 ((comment |>> (fun (range, str) -> CommentStatement({Position=range; Comment=str}))) <|> (leafValue |>> Value))
        .>> eof

    let statementlist = (many statement) .>> eof
    let all = ws >>. ((attempt valuelist) <|> statementlist)
