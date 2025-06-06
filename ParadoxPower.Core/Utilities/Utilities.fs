namespace ParadoxPower.Utilities

open System
open System.Collections.Concurrent
open System.Collections.Generic
open ParadoxPower.Utilities.Position
open System.Globalization
open System.IO

module Utils =

    let inline (==) (x: string) (y: string) =
        x.Equals(y, StringComparison.OrdinalIgnoreCase)

    type InsensitiveStringComparer() =
        interface IComparer<string> with
            member _.Compare(a, b) =
                String.Compare(a, b, StringComparison.OrdinalIgnoreCase)

    let memoize keyFunction memFunction =
        let dict = Dictionary<_, _>()

        fun n ->
            match dict.TryGetValue(keyFunction n) with
            | true, v -> v
            | _ ->
                let temp = memFunction n
                dict.Add(keyFunction n, temp)
                temp

    type LogLevel =
        | Silent
        | Normal
        | Verbose

    /// For the default logger only
    let mutable loglevel = Silent

    let logInner level message =
        match loglevel, level with
        | Silent, _ -> ()
        | Normal, Normal -> Printf.eprintfn "%s: %s" (DateTime.Now.ToString("HH:mm:ss")) message
        | Verbose, _ -> Printf.eprintfn "%s: %s" (DateTime.Now.ToString("HH:mm:ss")) message
        | _, _ -> ()

    let private defaultLogVerbose message = logInner Verbose message
    let private defaultLogNormal message = logInner Normal message

    let private defaultLogAll message =
        Printf.eprintfn "%s: %s" (DateTime.Now.ToString("HH:mm:ss")) message

    let mutable logDiag = defaultLogVerbose
    let mutable logInfo = defaultLogNormal
    let mutable logWarning = defaultLogNormal
    let mutable logError = defaultLogAll

    let log m = logInfo m

    let mkZeroFile file =
        mkRange file (mkPos 0 0) (mkPos 10000 0)

    let repeatN f n x =
        let mutable x = x

        for i = 1 to n do
            x <- f x

        x

    [<Literal>]
    let magicChar = '\u1E00'

module TryParser =
    let tryParseWith tryParseFunc =
        tryParseFunc
        >> function
            | true, v -> Some v
            | false, _ -> None

    let parseDate: string -> _ = tryParseWith DateTime.TryParse
    let parseInt: string -> _ = tryParseWith Int32.TryParse

    let parseIntWithDecimal: string -> _ =
        tryParseWith (fun s ->
            Int32.TryParse(
                s,
                NumberStyles.AllowDecimalPoint
                ||| NumberStyles.Integer,
                CultureInfo.InvariantCulture
            ))

    let parseSingle: string -> _ = tryParseWith Single.TryParse

    let parseDouble: string -> _ =
        tryParseWith (fun s ->
            Double.TryParse(
                s,
                (NumberStyles.Float ||| NumberStyles.AllowThousands),
                CultureInfo.InvariantCulture
            ))

    let parseDecimal: string -> _ =
        tryParseWith (fun s ->
            Decimal.TryParse(
                s,
                (NumberStyles.Float ||| NumberStyles.AllowThousands),
                CultureInfo.InvariantCulture
            ))
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|) = parseDate
    let (|Int|_|) = parseInt
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble


type StringToken = int
type StringLowerToken = int

type StringTokens =
    struct
        val lower: StringLowerToken
        val normal: StringToken
        /// We throw away the quotes when we intern, but we do need to keep that info, but don't want to have multiple tokens with/without quotes
        val quoted: bool

        new(lower, normal, quoted) =
            { lower = lower
              normal = normal
              quoted = quoted }
    end

type StringMetadata =
    struct
        val startsWithAmp: bool
        val containsDoubleDollar: bool
        val containsQuestionMark: bool
        val containsHat: bool
        val startsWithSquareBracket: bool
        val containsPipe: bool

        new(startsWithAmp,
            containsDoubleDollar,
            containsQuestionMark,
            containsHat,
            startsWithSquareBracket,
            containsPipe) =
            { startsWithAmp = startsWithAmp
              containsDoubleDollar = containsDoubleDollar
              containsQuestionMark = containsQuestionMark
              containsHat = containsHat
              startsWithSquareBracket = startsWithSquareBracket
              containsPipe = containsPipe }
    end

[<Sealed>]
type StringResourceManager() =
    // TODO: Replace with arrays?
    let strings = new ConcurrentDictionary<string, StringTokens>()
    let ints = new ConcurrentDictionary<StringToken, string>()

    let metadata = new ConcurrentDictionary<StringToken, StringMetadata>()

    let mutable i = 0
    let monitor = Object()

    member x.InternIdentifierToken(s) =
        let mutable res = Unchecked.defaultof<_>
        let ok = strings.TryGetValue(s, &res)

        if ok then
            res
        else
            lock monitor (fun () ->
                let retry = strings.TryGetValue(s, &res)

                if retry then
                    res
                else
                    let ls = s.ToLower().Trim('"')
                    let quoted = s.StartsWith "\"" && s.EndsWith "\""
                    let lok = strings.TryGetValue(ls, &res)

                    if lok then
                        let stringID = i
                        i <- i + 1
                        let resn = StringTokens(res.lower, stringID, quoted)
                        ints[stringID] <- s
                        metadata[stringID] <- metadata[res.lower]
                        strings[s] <- resn
                        resn
                    else
                        let stringID = i
                        let lowID = i + 1
                        i <- i + 2
                        let res = StringTokens(lowID, stringID, quoted)
                        let resl = StringTokens(lowID, lowID, false)


                        let (startsWithAmp,
                             containsQuestionMark,
                             containsHat,
                             containsDoubleDollar,
                             startsWithSquareBracket,
                             containsPipe) =
                            if ls.Length > 0 then
                                let startsWithAmp = ls[0] = '@'
                                let containsQuestionMark = ls.IndexOf('?') >= 0
                                let containsHat = ls.IndexOf('^') >= 0
                                let first = ls.IndexOf('$')
                                let last = ls.LastIndexOf('$')
                                let containsDoubleDollar = first >= 0 && first <> last
                                let startsWithSquareBracket = ls[0] = '[' || ls[0] = ']'
                                let containsPipe = ls.IndexOf('|') >= 0
                                // let quoted =
                                startsWithAmp,
                                containsQuestionMark,
                                containsHat,
                                containsDoubleDollar,
                                startsWithSquareBracket,
                                containsPipe
                            else
                                false, false, false, false, false, false

                        metadata[lowID] <-
                            StringMetadata(
                                startsWithAmp,
                                containsDoubleDollar,
                                containsQuestionMark,
                                containsHat,
                                startsWithSquareBracket,
                                containsPipe
                            )

                        metadata[stringID] <-
                            StringMetadata(
                                startsWithAmp,
                                containsDoubleDollar,
                                containsQuestionMark,
                                containsHat,
                                startsWithSquareBracket,
                                containsPipe
                            )

                        ints[lowID] <- ls
                        ints[stringID] <- s
                        strings[ls] <- resl
                        strings[s] <- res
                        res)
    member x.GetStringForIDs(id: StringTokens) = ints[id.normal]
    member x.GetLowerStringForIDs(id: StringTokens) = ints[id.lower]
    member x.GetStringForID(id: StringToken) = ints[id]
    member x.GetMetadataForID(id: StringToken) = metadata[id]
    member x.IntsCount = ints.Count
    member x.StringsCount = strings.Count
    member x.MetadataCount = metadata.Count

module StringResource =
    let mutable stringManager = StringResourceManager()

type StringTokens with

    member this.GetString() =
        StringResource.stringManager.GetStringForIDs this

    member this.GetMetadata() =
        StringResource.stringManager.GetMetadataForID this.normal