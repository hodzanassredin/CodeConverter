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
        pchar 'E' .>>. opt (pchar '+' <|> pchar '-') .>>. many1 digit
        <?> "ScaleFactor" 
    let real = 
        many1 digit .>>. pchar '.' .>>. many digit .>>. opt ScaleFactor
        <?> "real" 
    let integer = 
        let f = digit .>>. (opt hexDigit) .>>. (pchar 'H' <|> pchar 'L' )
        many1 digit <|> f
        <?> "integer" 
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
