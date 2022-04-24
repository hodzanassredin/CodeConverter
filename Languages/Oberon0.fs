namespace Languages
module Oberon0 = 
    open System
    open Parser

    type Symbol = 
        | Null | Times | Div | Mod | And | Plus | Minus | Or | Eql | Neq | Lss | Geq
        | Leq | Gtr | Period | Comma | Colon | RParen | RBrak | Of | Then | Do | LParen
        | LBrak | Not | Becomes | Number of Int64 | Ident of string | Semicolon | End | Else
        | Elsif | If | While | Array | Record | Const | Type | Var | Procedure
        | Begin | Module | Eof | Comment of string
    
    let keywords = [ 
        ("BY", Symbol.Null); 
        ("DO", Symbol.Do); 
        ("IF", Symbol.If); 
        ("IN", Symbol.Null); 
        ("IS", Symbol.Null); 
        ("OF", Symbol.Of); 
        ("OR", Symbol.Or); 
        ("TO", Symbol.Null); 
        ("END", Symbol.End); 
        ("FOR", Symbol.Null); 
        ("MOD", Symbol.Mod); 
        ("NIL", Symbol.Null); 
        ("VAR", Symbol.Var); 
        ("CASE", Symbol.Null); 
        ("ELSE", Symbol.Else); 
        ("EXIT", Symbol.Null); 
        ("THEN", Symbol.Then); 
        ("TYPE", Symbol.Type); 
        ("WITH", Symbol.Null); 
        ("ARRAY", Symbol.Array); 
        ("BEGIN", Symbol.Begin); 
        ("CONST", Symbol.Const); 
        ("ELSIF", Symbol.Elsif); 
        ("IMPORT", Symbol.Null); 
        ("UNTIL", Symbol.Null); 
        ("WHILE", Symbol.While); 
        ("RECORD", Symbol.Record); 
        ("REPEAT", Symbol.Null); 
        ("RETURN", Symbol.Null); 
        ("POINTER", Symbol.Null); 
        ("PROCEDURE", Symbol.Procedure); 
        ("DIV", Symbol.Div); 
        ("LOOP", Symbol.Null); 
        ("MODULE", Symbol.Module) ] |> Map.ofSeq

    let whitespace = 
        manyChars whitespaceChar 

    let symbol char (symbol:Symbol) = pchar char >>% symbol
    let symbol2 str symbol = pstring str |> mapP (fun _ -> symbol)
    let digit = satisfy (fun ch -> Char.IsDigit ch ) "digit"
    let letter = satisfy (fun ch -> Char.IsLetter ch ) "letter"
    let comment = 
        let (commentInt, commentIntRef) = createParserForwardedToRef<string>()
        commentIntRef := (pstring "*)" >>% "") <|> 
                         (pstring "(*" >>. commentInt) <|> 
                         (manyChars1 (notPchar '(' <|> notPchar '*') .>>. commentInt |> mapP (fun (a,b) -> a + b))
        pstring "(*" >>. commentInt |> mapP (fun s -> Symbol.Comment(s))

    let number = manyChars1 digit |> mapP Int64.Parse |> mapP Symbol.Number
    let ident = 
        letter .>>. manyChars (letter <|> digit) 
        |> mapP (fun (h,t)-> let s = sprintf "%c%s" h t
                             if keywords.ContainsKey s 
                             then keywords.[s]
                             else Symbol.Ident(s))

    let pSymbol = 
        ident <|>
        comment <|>
        number <|>
        symbol '$' Symbol.And <|>
        symbol '*' Symbol.Times <|>
        symbol '+' Symbol.Plus <|>
        symbol '-' Symbol.Minus <|>
        symbol '=' Symbol.Eql <|>
        symbol '#' Symbol.Neq <|>
        symbol2 "<=" Symbol.Leq <|>
        symbol '<' Symbol.Lss <|>
        symbol2 ">=" Symbol.Geq <|>
        symbol ';' Symbol.Semicolon <|>
        symbol ',' Symbol.Comma <|>
        symbol2 ":=" Symbol.Becomes <|>
        symbol ':' Symbol.Colon <|>
        symbol '.' Symbol.Period <|>
        symbol '(' Symbol.LParen <|>
        symbol ')' Symbol.RParen <|>
        symbol '[' Symbol.LBrak <|>
        symbol ']' Symbol.RBrak <|>
        symbol '~' Symbol.Not <|>
        (anyPchar >>% Symbol.Null) |> many
        
     
