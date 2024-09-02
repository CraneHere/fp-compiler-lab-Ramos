module Types

type input_handler = unit -> string
type output_handler = string -> unit

// Objects that store actual evaluated data - integers, strings, functions etc. There's no need to evaluate them further.
type data_object =
    | Int of int
    | String of string
    | Null
    | Undefined
    | ComplexObject of Map<string, data_object>

    | Function of string*expr_ast*environment
    
    | Writer of output_handler
    | Reader of input_handler
// Objects that should be evaluated.
and expr_ast =
    | DataObject of data_object            // Hardcoded data object ("1", '"string"', "$null", "$undefined", " { a:1, b:{c:3, d:$null} } ")
    | Var of string list                   // Var is a list of strings, for example var  someObject.someField.first is list of ["someObject", "someField", "first"]
    | Operator of string*expr_ast*expr_ast // "(<op-name> <op1> <op2>)"
    | If of expr_ast*expr_ast*expr_ast     // "if <expr> then <expr> else <expr>"
    | Lambda of string*expr_ast            // "lam <id> -> <expr>"
    | Apply of expr_ast*expr_ast list      // "(<expr> <expr1> <expr2> ... <exprn>)"
    | LocalLet of string*expr_ast*expr_ast // "loclet <id> = <expr> in <expr>"

    | Query of expr_ast*string*expr_ast option // "? <expr> <str> [<expr>]"

    // Std funcs:
    | Print of expr_ast list
    | Println of expr_ast list
    | ReadString
    | ReadInt
    | ToInt of expr_ast

    // File reading and writing
    | CreateFileReader of expr_ast
    | CreateFileWriter of expr_ast
    | Read of expr_ast
    | Write of expr_ast*expr_ast

    | UnresolvedComplexObject of Map<string, expr_ast>
// Statements that program is made of.
and stmt =
    | Let of string*string list*expr_ast
    | NormalExpr of expr_ast
// Results of statement processing.
and stmt_result =
    | LetResult of string*data_object
    | NormalExprResult of data_object
and environment = Map<string, data_object>

type parser_result =
    | Error of string
    | Success of stmt list

type io_handler =
    input_handler * output_handler // Input function handler * output function handler

let io_r ioh = fst ioh
let io_w ioh = snd ioh