namespace ParadoxPower.Parser

open ParadoxPower.Languages
open ParadoxPower.Process
open FParsec
open ParadoxPower.Utilities.Position
open ParadoxPower.Utilities.Utils

module internal SharedParsers =
    let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
        fun stream ->
            log $"%A{stream.Position}: Entering %s{label}"
            let reply = p stream
            log $"%A{stream.Position}: Leaving %s{label} (%A{reply.Status})"
            reply

    let betweenL (popen: Parser<_, _>) (pclose: Parser<_, _>) (p: Parser<_, _>) (label: string) =
        let notClosedError =
            messageError (System.String.Format(Resources.Parse_NotClosed, label))

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
                        Reply(reply3.Status, mergeErrors error3 notClosedError)
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

    let isAnyOfIdCharArray = isAnyOf idCharArray
    let isIdChar = fun c -> isLetter c || isDigit c || isAnyOfIdCharArray c

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
           '('
           ')'
           magicChar |]

    let isAnyValueChar = isAnyOf valueCharArray
    let isValueChar = fun c -> isLetter c || isDigit c || isAnyValueChar c


    // Utility parsers
    // =======
    let whiteSpace = spaces <?> Resources.Parse_Whitespace

    let str s =
        pstring s .>> whiteSpace <?> $"{Resources.Parse_String} {s}"

    let strSkip s =
        skipString s .>> whiteSpace <?> ("skip string " + s)

    let ch c =
        pchar c .>> whiteSpace <?> ("char " + string c)

    let chSkip c =
        skipChar c .>> whiteSpace <?> ("skip char " + string c)

    let clause inner =
        betweenL
            (chSkip '{' <?> Resources.Parse_OpeningBrace)
            (skipChar '}' <?> Resources.Parse_ClosingBrace)
            inner
            Resources.Parse_Clause

    let quotedCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = (stringReturn "\\\"" "\"" <|> pstring "\\")
    let metaprogrammingCharSnippet = many1Satisfy (fun c -> c <> ']' && c <> '\\')

    let getRange (start: Position) (endp: Position) =
        mkRange
            start.StreamName
            (mkPos (int start.Line) (int start.Column - 1))
            (mkPos (int endp.Line) (int endp.Column - 1))

    let parseWithPosition p =
        pipe3 getPosition p getPosition (fun s r e -> getRange s e, r)

    // Base types
    // =======
    let oppLTE = skipString "<=" >>% Operator.LessThanOrEqual
    let oppGTE = skipString ">=" >>% Operator.GreaterThanOrEqual
    let oppNE = skipString "!=" >>% Operator.NotEqual
    let oppEE = skipString "==" >>% Operator.EqualEqual
    let oppQE = skipString "?=" >>% Operator.QuestionEqual
    let oppLT = skipChar '<' >>% Operator.LessThan
    let oppGT = skipChar '>' >>% Operator.GreaterThan
    let oppE = skipChar '=' >>% Operator.Equals

    let operator =
        choiceL [ oppLTE; oppGTE; oppNE; oppEE; oppLT; oppGT; oppE; oppQE ] Resources.Parse_Operator
        .>> whiteSpace

    let operatorLookahead =
        choice [ chSkip '='; chSkip '>'; chSkip '<'; chSkip '!'; strSkip "?=" ]
        <?> $"{Resources.Parse_Operator} 1"

    let comment =
        parseWithPosition (skipChar '#' >>. restOfLine true .>> whiteSpace)
        <?> Resources.Parse_Comment

    let key = (many1SatisfyL isIdChar "id character") .>> whiteSpace |>> Key <?> "id"
 
    let keyQ: Parser<Key,unit> =
        between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar))
        .>> whiteSpace
        |>> (fun s -> Key("\"" + s + "\""))
        <?> "quoted key"

    let valueStr =
        (many1SatisfyL isValueChar "value character") |>> String
        <?> Resources.Parse_String

    let valueQ =
        betweenL (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar)) Resources.Parse_QuotedString
        |>> QString
        <?> Resources.Parse_QuotedString

    // NOTE: yes 和 no 是大小写敏感的
    let valueBYes: Parser<Value,unit> =
        skipString "yes" .>> nextCharSatisfiesNot isValueChar >>% Bool(true)

    let valueBNo =
        skipString "no" .>> nextCharSatisfiesNot isValueChar >>% Bool(false)

    let valueInt = pint64 .>> nextCharSatisfiesNot isValueChar |>> (fun int64 -> Int(int int64))
    let valueFloat = pfloat .>> nextCharSatisfiesNot isValueChar |>> (fun f -> Float (decimal f))

    let hsvI =
        clause (
            pipe4
                (parseWithPosition valueFloat .>> whiteSpace)
                (parseWithPosition valueFloat .>> whiteSpace)
                (parseWithPosition valueFloat .>> whiteSpace)
                (opt (parseWithPosition valueFloat .>> whiteSpace))
                (fun a b c d ->
                    match (a, b, c, d) with
                    | a, b, c, Some d ->
                        Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ]
                    | a, b, c, None -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    // 我们将 hsv 和 rgb 这一类的附加值当作 Leaf 处理
    let hsvCore (typeName: string) =
        pipe3
            (parseWithPosition (pstring typeName .>> whiteSpace))
            (opt (parseWithPosition (pstring "360" .>> whiteSpace)))
            hsvI
            (fun (pos, hsvText) hueMode color ->
                match color with
                | Clause clause ->
                    match hueMode with
                    | Some(hueModePos, hueModeText) ->
                        Clause(
                            Statement.KeyValue(
                                PosKeyValue(pos, KeyValueItem(Key(hsvText), Value.String(hsvText), Operator.Equals))
                            )
                            :: Statement.KeyValue(
                                PosKeyValue(
                                    hueModePos,
                                    KeyValueItem(Key(hueModeText), Value.String(hueModeText), Operator.Equals)
                                )
                            )
                            :: clause
                        )
                    | None ->
                        Clause(
                            Statement.KeyValue(
                                PosKeyValue(pos, KeyValueItem(Key(hsvText), Value.String(hsvText), Operator.Equals))
                            )
                            :: clause
                        )
                | _ -> failwith "assert")
        <?> typeName

    let hsv = hsvCore "hsv"
    let hsvC = hsvCore "HSV"

    let rgbI =
        clause (
            pipe4
                (parseWithPosition valueInt .>> whiteSpace)
                (parseWithPosition valueInt .>> whiteSpace)
                (parseWithPosition valueInt .>> whiteSpace)
                (opt (parseWithPosition valueInt .>> whiteSpace))
                (fun a b c d ->
                    match (a, b, c, d) with
                    | a, b, c, Some d ->
                        Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ]
                    | a, b, c, None -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    let rgbCore (typeName: string) =
        pipe2 (parseWithPosition (pstring typeName .>> whiteSpace)) rgbI (fun (pos, text) color ->
            match color with
            | Clause clause ->
                Clause(
                    Statement.KeyValue(PosKeyValue(pos, KeyValueItem(Key(text), Value.String(text), Operator.Equals)))
                    :: clause
                )
            | _ -> failwith "assert")
        <?> typeName

    let rgb = rgbCore "rgb"

    let rgbC = rgbCore "RGB"

    let metaPrograming =
        pipe3 (pstring "@\\[") metaprogrammingCharSnippet (ch ']') (fun a b c -> (a + b + string c))
        |>> String
    // Complex types
    // =======

    // Recursive types
    let keyValue, keyvalueimpl = createParserForwardedToRef ()
    let (value: Parser<Value, unit>), valueimpl = createParserForwardedToRef ()

    let leafValue =
        pipe3 getPosition (value .>> whiteSpace) getPosition (fun a b c -> (getRange a c, b))

    let leafValues =
        pipe3 getPosition value getPosition (fun a b c -> (getRange a c, b))

    let statement =
        comment
        |>> (fun (range, str) -> CommentStatement({ Position = range; Comment = str }))
        <|> (attempt (leafValues .>> whiteSpace .>> notFollowedBy operatorLookahead |>> Value))
        <|> keyValue
        <?> Resources.Parse_Statement

    let valueClause = clause (many statement) |>> Clause

    let valueCustom: Parser<Value, unit> =
        let vcP = valueClause
        let iP = attempt valueInt
        let fP = attempt valueFloat
        let byP = attempt valueBYes <|> valueStr
        let bnP = attempt valueBNo <|> valueStr
        let mpP = metaPrograming
        let rgbP = attempt rgb <|> valueStr
        let rgbCP = attempt rgbC <|> valueStr
        let hsvP = attempt hsv <|> valueStr
        let hsvCP = attempt hsvC <|> valueStr

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
                    if f.Status = Ok then f else valueStr stream
            | _ ->
                match stream.PeekString 3, stream.PeekString 2 with
                | "rgb", _ -> rgbP stream
                | "RGB", _ -> rgbCP stream
                | "hsv", _ -> hsvP stream
                | "HSV", _ -> hsvCP stream
                | "yes", _ -> byP stream
                | _, "no" -> bnP stream
                | "@\\[", _ -> mpP stream
                | _ -> valueStr stream

    valueimpl.Value <- valueCustom <?> "value"

    keyvalueimpl.Value <-
        pipe5 getPosition (keyQ <|> key) operator value (getPosition .>> whiteSpace) (fun start id op value endp ->
            KeyValue(PosKeyValue(getRange start endp, KeyValueItem(id, value, op))))

    let alle = whiteSpace >>. many statement .>> eof |>> ParsedFile

    let valueList =
        many1 (
            (comment
             |>> (fun (range, str) -> CommentStatement({ Position = range; Comment = str })))
            <|> (leafValue |>> Value)
        )
        .>> eof

    let statementList = (many statement) .>> eof
    let all = whiteSpace >>. ((attempt valueList) <|> statementList)
