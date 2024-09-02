module Parser

open FParsec
open Types

type UserState = unit
type Parser<'t> = Parser<'t, UserState> 

let test p str =
    match run p str with
    | ParserResult.Success(result, _, _)   -> printfn "Success: %A" result
    | ParserResult.Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parser_expr, parser_expr_ref = createParserForwardedToRef<expr_ast, unit>()

let private ws : Parser<_> = spaces <|> eof

let private identifier : Parser<_> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

let private stringLiteral : Parser<_> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

// Parser for hardcoded data objects
let private parser_data_obj_int = (pint32 .>> ws) |>> Int |>> DataObject
let private parser_data_obj_string = (stringLiteral .>> ws) |>> String |>> DataObject
let private parser_data_obj_null = (pstring "$null" .>> ws) >>% Null |>> DataObject
let private parser_data_obj_undefined = (pstring "$undefined" .>> ws) >>% Undefined |>> DataObject

// Lambda func
let private parser_lambda = pipe2 (pstring "lam" >>. ws >>. identifier .>> ws .>> pstring "->" .>> ws) (parser_expr .>> ws) (fun x y -> Lambda(x, y))

// Parsing complex object
let private make_map_from_pairs pairs = List.fold (fun x y -> match y with (y1, y2) -> Map.add y1 y2 x) Map.empty pairs

let private parser_data_obj_complex_parse_entry = pipe2 (identifier .>> ws .>> pstring ":" .>> ws) (parser_expr .>> ws) (fun x y -> (x, y)) 
let private parser_data_obj_complex = between
                                        (pstring "{" .>> ws)
                                        (pstring "}" .>> ws)
                                        (sepBy parser_data_obj_complex_parse_entry (pstring "," .>> ws)) |>> make_map_from_pairs |>> UnresolvedComplexObject

// Parsing operator apply
let private parser_operator_symbol : Parser<_> =
   let predicate = isAnyOf (['+'; '-'; '*'; '/'; '='; '<'; '>'; '!']) in
   many1Satisfy predicate

let private parser_operator_apply = between (pstring "(" .>> ws) (pstring ")" .>> ws) (pipe3 (parser_operator_symbol .>> ws) (parser_expr .>> ws) (parser_expr .>> ws) (fun x y z -> Operator(x, y, z)))

// Parsing var
let private parser_var = (sepBy1 (identifier) (pstring ".")) .>> ws |>> Var

// Std funcs
let private parser_print = between (pstring "(" .>> ws) (pstring ")" .>> ws) (pstring "print" >>. ws >>. (many (parser_expr .>> ws))) |>> Print
let private parser_println = between (pstring "(" .>> ws) (pstring ")" .>> ws) (pstring "println" >>. ws >>. (many (parser_expr .>> ws))) |>> Println

// Parsing if
let private parser_if = pipe3 (pstring "if" >>. ws >>. parser_expr .>> ws) (pstring "then" >>. ws >>. parser_expr .>> ws) (pstring "else" >>. ws >>. parser_expr .>> ws) (fun x y z -> If(x, y, z))

// Parsing apply
let private parser_apply = between (pstring "(" .>> ws) (pstring ")" .>> ws) (pipe2 (parser_expr .>> ws) (many (parser_expr .>> ws)) (fun x y -> Apply(x, y)))

// Local let
let private parser_loclet = pipe3 (pstring "loclet" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws) 
                                     (parser_expr .>> ws .>> pstring "in" .>> ws)
                                     (parser_expr .>> ws) (fun x y z -> LocalLet(x, y, z))

let private parser_query_is = pipe2 (pstring "?" >>. ws >>. parser_expr .>> ws .>> pstring "is" .>> ws) (parser_expr .>> ws) (fun x y -> Query(x, "is", Some(y))) 
let private parser_query_left = pipe2 (pstring "?" >>. ws >>. parser_expr .>> ws) (identifier .>> ws) (fun x y -> Query(x, y, None) )

// ReadString, ReadInt
let private parser_read_string = (pstring "read_string" .>> ws) >>% ReadString
let private parser_read_int = (pstring "read_int" .>> ws) >>% ReadInt
let private parser_to_int = (pstring "to_int" >>. ws >>. parser_expr .>> ws) |>> ToInt 

// Files readers & writers
let private parser_create_file_reader = (pstring "$file_reader" >>. ws >>. parser_expr .>> ws) |>> CreateFileReader
let private parser_create_file_writer = (pstring "$file_writer" >>. ws >>. parser_expr .>> ws) |>> CreateFileWriter
let private parser_read = (pstring "$read" >>. ws >>. parser_expr .>> ws) |>> Read
let private parser_write_to = pipe2 (pstring "$write" >>. ws >>. parser_expr .>> ws) (pstring "$to" >>. ws >>. parser_expr .>> ws)
                                    (fun x y -> Write(x, y))

// Most important parser
do parser_expr_ref := choice [
    parser_data_obj_null
    parser_data_obj_int
    parser_data_obj_string
    parser_data_obj_complex
    parser_data_obj_undefined

    attempt parser_create_file_reader
    parser_create_file_writer
    parser_read
    parser_write_to

    attempt parser_read_string
    attempt parser_read_int
    attempt parser_loclet
    attempt parser_operator_apply
    attempt parser_println
    attempt parser_print
    attempt parser_if
    attempt parser_lambda
    attempt parser_query_is
    attempt parser_query_left
    attempt parser_to_int

    parser_apply
    parser_var
]

let parser_stmt_let = pipe3 (pstring "let" >>. ws >>. identifier .>> ws) (many (identifier .>> ws)) (pstring "=" >>. ws >>. parser_expr) (fun x y z -> stmt.Let(x, y, z)) 
let parser_stmt_normal_expr = (parser_expr .>> ws) |>> stmt.NormalExpr

let parser_stmt = choice [
    parser_stmt_let
    parser_stmt_normal_expr
]

let parser_stmt_list = ws >>. (many (parser_stmt .>> (pstring ";" .>> ws)))

let run_main_parser str =
    match (run parser_stmt_list str) with
    | ParserResult.Failure(x, _, _) -> parser_result.Error(x)
    | ParserResult.Success(x, _, _) -> parser_result.Success(x)
