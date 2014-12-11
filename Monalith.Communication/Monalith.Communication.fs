namespace Monalith.Communication

open System
open System.IO
open System.Net
open System.Text
open System.Text.RegularExpressions

type MoBoard =
    { Uri:  Uri
      Name: string }

type MoCategory =
    { Name: string
      Boards: MoBoard[] }

type MoThread =
    { Id:              string
      Name:            string
      DateTimeCreated: DateTimeOffset
      ResponseCount:   int
      Uri:             Uri }

type MoPost =
    { UserName:       string
      UserId:         string
      DateTimePosted: DateTimeOffset
      Email:          string
      Body:           string }

module Parser =

    let private matchedValue (matchResult:Match) (key:String) =
        matchResult.Groups.[key].Value.Trim() |> WebUtility.HtmlDecode

    let (|CategoryLiteral|_|) text =
        let regexCategory = Regex (@"<BR><BR><B>(?<name>.*)</B><BR>", RegexOptions.IgnoreCase)
        let categoryMatchResult = regexCategory.Match(text)
        if categoryMatchResult.Success then
            Some (matchedValue categoryMatchResult "name")
        else
            None

    let (|BoardLiteral|_|) text =
            let regexBoard = Regex (@"^<A HREF=(?<uri>[^>]+)>(?<name>.*)</A>", RegexOptions.IgnoreCase)
            let boardMatchResult = regexBoard.Match(text)
            if boardMatchResult.Success then
                let m = matchedValue boardMatchResult
                let uri = Uri(m "uri")
                Some (uri, m "name")
            else
                None

    let (|ThreadLiteral|_|) text =
        let toDateTimeOffset text =
            let epochTimeOrigin = DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero)
            text
            |> Double.Parse
            |> epochTimeOrigin.AddSeconds
        let regexThread = Regex @"^((?<id>[\d]+)\.dat)<>(?<name>.+) \((?<count>[\d]+)\)"
        let matchResult = regexThread.Match(text)
        if matchResult.Success then
            let m = matchedValue matchResult
            Some (m "id", m "name", m "id" |> toDateTimeOffset, m "count" |> Int32.Parse)
        else
            None

    let (|PostLiteral|_|) text =
        let postRegex =
            Regex (
                @"^(?<name>.+)" +
                @"\<\>" +
                @"(?<email>[^\<]*)" +
                @"\<\>" +
                @"(?<year>\d+)/(?<month>\d+)/(?<day>\d+)\(.\) " +
                @"(?<hour>\d+):(?<minute>\d+):(?<second>\d+)(\.\d+)?" +
                @"( ID:(?<id>[^\<]+))?" +
                @"\<\>" +
                @"(?<body>.+)\<\>", RegexOptions.IgnoreCase)
        let matchResult = postRegex.Match(text)
        if matchResult.Success then
            let ms = matchedValue matchResult
            let mi = ms >> Int32.Parse
            let dateTimePosted =
                let normalizeYear year = Math.Min(year, 1900 + year)
                let year = mi "year" |> normalizeYear
                let jstTimeDiff = TimeSpan.FromHours(9.0)
                DateTimeOffset (year, mi "month", mi "day", mi "hour", mi "minute", mi "second", 0, jstTimeDiff)
            Some (ms "name", ms "email", dateTimePosted, ms "id", ms "body")
        else
            None

    type private Token =
        | CategoryTitle of string
        | Board of Uri * string

    let parseRootMenu (lines:string seq) =

        let tokenize (lines:string seq) =
            let toToken (text:string) =
                match text with
                | CategoryLiteral title -> Some (CategoryTitle(title))
                | BoardLiteral (uri, title) -> Some (Board(Uri(uri, "subject.txt"), title))
                | _ -> None
            let tokens =
                lines
                |> Seq.map(toToken)
                |> Seq.filter(fun t -> t.IsSome)
                |> Seq.map(fun t -> t.Value)
            [ for token in tokens do yield token ]

        let rec parseBoardList tokens =
            match tokens with
            | Board (uri, name) :: tokens ->
                let board = { Uri = uri; Name = name } : MoBoard
                let list, tokens = parseBoardList tokens
                (board::list, tokens)
            | _ ->
                ([], tokens)
            
        let rec parseCategoryList tokens =
            match tokens with
            | CategoryTitle name :: tokens ->
                let boardList, tokens = parseBoardList tokens
                let category = { Name = name; Boards = (boardList |> List.toArray) }
                let list, tokens = parseCategoryList tokens
                (category::list, tokens)
            | _ -> ([], tokens)

        let parseInput (tokens: Token list) =
            let _, tokens = parseBoardList tokens
            let categories, tokens = parseCategoryList tokens
            categories

        lines
        |> tokenize
        |> parseInput
        |> List.toSeq

    let parseThreads (baseUri:Uri) (lines:string seq) =
        let parseThread (text:string) =
            match text with
            | ThreadLiteral (id, name, dateTime, count) ->
                let uri = Uri (baseUri, String.Format("dat/{0}.dat", id))
                Some { Id = id; Name = name; DateTimeCreated = dateTime; ResponseCount = count; Uri = uri }
            | _ -> None
        lines
        |> Seq.map(parseThread)
        |> Seq.filter(fun p -> p.IsSome)
        |> Seq.map(fun p-> p.Value)

    let parsePosts (lines:string seq) =
        let parsePost (text:string) =
            match text with
            | PostLiteral (name, email, dateTime, id, body) ->
                Some { UserName = name; Email = email; DateTimePosted = dateTime; UserId = id; Body = body  }
            | _ -> None
        lines
        |> Seq.map(parsePost)
        |> Seq.filter(fun p -> p.IsSome)
        |> Seq.map(fun p-> p.Value)

module HttpClient =

    let private loadAsync parser (uri:Uri) =
        let toSeq (reader:TextReader) =
            Seq.initInfinite(fun _ -> reader.ReadLine())
            |> Seq.takeWhile(fun line -> line <> null)
        async {
            let sjisEncoding = Encoding.GetEncoding("SJIS")
            let req = WebRequest.Create(uri)
            let! res = req.AsyncGetResponse()
            use stream = res.GetResponseStream()
            use reader = new StreamReader (stream, sjisEncoding)
            return
                reader
                |> toSeq
                |> Seq.toArray
                |> parser
        }

    let getPostsAsync (thread:MoThread) =
        thread.Uri
        |> loadAsync Parser.parsePosts

    let getThreadsAsync (board:MoBoard) =
        board.Uri
        |> loadAsync (Parser.parseThreads board.Uri)

    let getDefaultCategoriesAsync =
        Uri "http://menu.2ch.net/bbsmenu.html"
        |> loadAsync Parser.parseRootMenu

