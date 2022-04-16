namespace Languages

module CPParser =
    open System
    open Parser
    open CPAst    
    

        
    
    //let NIL =
    //    pstring "NIL"
    //    >>% CPAst.NIL   // map to JNull
    //    <?> "NIL"  // give it a label
    
    let digit =
        satisfy (fun ch -> Char.IsDigit ch ) "digit"
    let letter = 
        satisfy (fun ch -> Char.IsLetter ch ) "digit"
    let underscore = pchar '_'
    let ident =
        (letter <|> underscore) .>>. many (letter <|> underscore <|> digit)
        <?> "ident" 
    let hexDigit = 
        digit <|> pchar 'A' <|> pchar 'B' <|> pchar 'C' <|> pchar 'D' <|> pchar 'E' <|> pchar 'F'
        <?> "hexDigit" 
    let ScaleFactor = 
        let toStr ((e,sign), scale) = 
            match sign with
                | Some(sign)-> sprintf "%c%c%s" e sign scale 
                | None -> sprintf "%c%s" e scale 
            
        pchar 'E' .>>. opt (pchar '+' <|> pchar '-') .>>. manyChars1 digit |> mapP toStr
        <?> "ScaleFactor" 

    let parseChoice2 (str:string) (tryParse: string -> (Boolean * 'a)) (tryParse2: string -> (bool * 'b)) (failStr:string) =
        match tryParse str with
        | true,c1 -> Choice1Of2 c1
        | _ -> match tryParse2 str with
               | true,c2 -> Choice2Of2 c2
               | _ -> failwith failStr

    let real = 
        let parseFloatChoice (str:string) = parseChoice2 str System.Single.TryParse System.Double.TryParse (sprintf "Cant parse real '%s'" str)
        let parseFloat (((digits, dot), decs), scale) = 
            let floatStr : string = sprintf "%s.%s%s" digits decs (match scale with | Some(scale) -> scale | None -> "")
            parseFloatChoice floatStr
            
        manyChars1 digit .>>. pchar '.' .>>. manyChars digit .>>. opt ScaleFactor |> mapP parseFloat
        <?> "real" 

    
    let integer = 
        let hexDigit = digit .>>. (manyChars hexDigit) .>>. (pchar 'H' <|> pchar 'L' )
        let parseHexDigit ((d:Char,hex: string), flag: Char) = 
            let hexStr = string d + hex
            if flag = 'H' then match System.Int64.TryParse hexStr with
                                | true,long -> Choice2Of2 long
                                | _ -> failwithf "Cant parse hex number '%s'" hexStr
            elif flag = 'L' then 
                match System.Int32.TryParse hexStr with
                | true,int -> Choice1Of2 int
                | _ -> failwithf "Cant parse hex number '%s'" hexStr
            else failwithf "Cant parse hex number '%s'" hexStr
        
        let parseInt (str:string) = parseChoice2 str System.Int32.TryParse System.Int64.TryParse (sprintf "Cant parse integer '%s'" str)

        (manyChars1 digit |> mapP parseInt) <|> (hexDigit  |> mapP parseHexDigit)
        <?> "integer" 

    let number = 
        integer |> mapP Choice1Of2 <|> (real |> mapP Choice2Of2)
        <?> "number" 

    let character = 
        digit .>>. manyChars hexDigit .>>. pchar 'X' |> mapP (fun ((d,digits), x)-> System.Convert.ToChar(System.Convert.ToUInt32(sprintf "%c%s" d digits, 16)))
        <?> "character" 
    // ======================================
    // Parsing a JBool
    // ======================================
    
    //let jBool =
    //    let jtrue =
    //        pstring "true"
    //        >>% CPAst. true   // map to JBool
    //    let jfalse =
    //        pstring "false"
    //        >>% JBool false  // map to JBool
    
    //    // choose between true and false
    //    jtrue <|> jfalse
    //    <?> "bool"           // give it a label
    
    
    //// ======================================
    //// Parsing a JString
    //// ======================================
    
    ///// Parse an unescaped char
    //let jUnescapedChar =
    //    satisfy (fun ch -> ch <> '\\' && ch <> '\"') "char"
    
    ///// Parse an escaped char
    //let jEscapedChar =
    //    [
    //    // (stringToMatch, resultChar)
    //    ("\\\"",'\"')      // quote
    //    ("\\\\",'\\')      // reverse solidus
    //    ("\\/",'/')        // solidus
    //    ("\\b",'\b')       // backspace
    //    ("\\f",'\f')       // formfeed
    //    ("\\n",'\n')       // newline
    //    ("\\r",'\r')       // cr
    //    ("\\t",'\t')       // tab
    //    ]
    //    // convert each pair into a parser
    //    |> List.map (fun (toMatch,result) ->
    //        pstring toMatch >>% result)
    //    // and combine them into one
    //    |> choice
    //    <?> "escaped char" // set label
    
    ///// Parse a unicode char
    //let jUnicodeChar =
    
    //    // set up the "primitive" parsers
    //    let backslash = pchar '\\'
    //    let uChar = pchar 'u'
    //    let hexdigit = 
    //        anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])
    //    let fourHexDigits =
    //        hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
    
    //    // convert the parser output (nested tuples)
    //    // to a char
    //    let convertToChar (((h1,h2),h3),h4) =
    //        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
    //        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char
    
    //    // set up the main parser
    //    backslash  >>. uChar >>. fourHexDigits
    //    |>> convertToChar
    
    
    ///// Parse a quoted string
    //let quotedString =
    //    let quote = pchar '\"' <?> "quote"
    //    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar
    
    //    // set up the main parser
    //    quote >>. manyChars jchar .>> quote
    
    ///// Parse a JString
    //let jString =
    //    // wrap the string in a JString
    //    quotedString
    //    |>> JString           // convert to JString
    //    <?> "quoted string"   // add label
    
    //// ======================================
    //// Parsing a JNumber
    //// ======================================
    
    ///// Parse a JNumber
    //let cpNumber =
    
    //    // set up the "primitive" parsers
    //    let optSign = opt (pchar '-')
    
    //    let zero = pstring "0"
    
    //    let digitOneNine =
    //        satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "1-9"
    
    //    let digit =
    //        satisfy (fun ch -> Char.IsDigit ch ) "digit"
    
    //    let point = pchar '.'
    
    //    let e = pchar 'e' <|> pchar 'E'
    
    //    let optPlusMinus = opt (pchar '-' <|> pchar '+')
    
    //    let nonZeroInt =
    //        digitOneNine .>>. manyChars digit
    //        |>> fun (first,rest) -> string first + rest
    
    //    let intPart = zero <|> nonZeroInt
    
    //    let fractionPart = point >>. manyChars1 digit
    
    //    let exponentPart = e >>. optPlusMinus .>>. manyChars1 digit
    
    //    // utility function to convert an optional value to a string, or "" if missing
    //    let ( |>? ) opt f =
    //        match opt with
    //        | None -> ""
    //        | Some x -> f x
    
    //    let convertToJNumber (((optSign,intPart),fractionPart),expPart) =
    //        // convert to strings and let .NET parse them! - crude but ok for now.
    
    //        let signStr =
    //            optSign
    //            |>? string   // e.g. "-"
    
    //        let fractionPartStr =
    //            fractionPart
    //            |>? (fun digits -> "." + digits )  // e.g. ".456"
    
    //        let expPartStr =
    //            expPart
    //            |>? fun (optSign, digits) ->
    //                let sign = optSign |>? string
    //                "e" + sign + digits          // e.g. "e-12"
    
    //        // add the parts together and convert to a float, then wrap in a JNumber
    //        (signStr + intPart + fractionPartStr + expPartStr)
    //        |> float
    //        |> JNumber
    
    //    // set up the main parser
    //    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    //    |>> convertToJNumber
    //    <?> "number"   // add label
    
    //// ======================================
    //// Parsing a JArray
    //// ======================================
    
    //let jArray =
    
    //    // set up the "primitive" parsers
    //    let left = pchar '[' .>> spaces
    //    let right = pchar ']' .>> spaces
    //    let comma = pchar ',' .>> spaces
    //    let value = jValue .>> spaces
    
    //    // set up the list parser
    //    let values = sepBy value comma
    
    //    // set up the main parser
    //    between left values right
    //    |>> JArray
    //    <?> "array"
    
    //// ======================================
    //// Parsing a JObject
    //// ======================================
    
    
    //let jObject =
    
    //    // set up the "primitive" parsers
    //    let left = spaces >>. pchar '{' .>> spaces
    //    let right = pchar '}' .>> spaces
    //    let colon = pchar ':' .>> spaces
    //    let comma = pchar ',' .>> spaces
    //    let key = quotedString .>> spaces
    //    let value = jValue .>> spaces
    
    //    // set up the list parser
    //    let keyValue = (key .>> colon) .>>. value
    //    let keyValues = sepBy keyValue comma
    
    //    // set up the main parser
    //    between left keyValues right
    //    |>> Map.ofList  // convert the list of keyValues into a Map
    //    |>> JObject     // wrap in JObject
    //    <?> "object"    // add label
    
    //// ======================================
    //// Fixing up the jValue ref
    //// ======================================
    
    //// fixup the forward ref
    //jValueRef := choice
    //    [
    //    jNull
    //    jBool
    //    jNumber
    //    jString
    //    jArray
    //    jObject
    //    ]
