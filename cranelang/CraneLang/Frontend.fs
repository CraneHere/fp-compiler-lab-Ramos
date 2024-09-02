module Frontend

open Core
open Parser
open Types

open FParsec
open System
open System.IO

// ### CONFIG TYPES
// ###
type config_input =
    | Stdin
    | File of StreamReader

type config_output =
    | Stdout
    | File of StreamWriter

// Type defines configuration of program. (Source of input data etc.)
type config =
    | Interactive
    | SourceCode of string*config_input*config_output

let config_default = Interactive

// ### PARSING COMMAND LINE OPTIONS
// ### 
let cfg_define_f_opt cfg src =
    match cfg with
    | Interactive -> SourceCode(src, Stdin, Stdout)
    | _ as x -> x

let cfg_define_i_opt cfg (fname:string) =
    match cfg with
    | Interactive -> failwith "Can't use -i option with interactive mode"
    | SourceCode(x, _, y) -> SourceCode(x, config_input.File(new StreamReader(fname) ), y)

let cfg_define_o_opt cfg (fname:string) =
    match cfg with
    | Interactive -> failwith "Can't use -o option with interactive mode"
    | SourceCode(x, y, _) -> SourceCode(x, y, config_output.File(new StreamWriter(fname) ))

let parse_args (args:string list) : config =
    let rec _parse_args_raw (args:string list) (acc_cfg:config) =
        let handle_opt_with_one_arg opt_name cfg_define_func tail =
            match tail with
              | [] -> failwith (sprintf "Fatal: Option %s requires one argument!" opt_name)
              | h::t -> _parse_args_raw t (cfg_define_func acc_cfg h)
        match args with
        | [] -> acc_cfg
        | h::t -> match h with
                    | "-f" -> handle_opt_with_one_arg "-f" cfg_define_f_opt t
                    | "-i" -> handle_opt_with_one_arg "-i" cfg_define_i_opt t
                    | "-o" -> handle_opt_with_one_arg "-o" cfg_define_o_opt t
                    | _ as opt -> failwith (sprintf "Don't know opt %s" opt)
    in _parse_args_raw args config_default

// ### CREATE IO HANDLER FROM CONFIG
// ### 
let create_io_handler cfg =
    let stdin_handler = (Console.ReadLine)
    let stdout_handler = (printf "%s")
    match cfg with
    | Interactive -> io_handler(stdin_handler, stdout_handler)
    | SourceCode(_, input, output) ->
        let _inp = match input with Stdin -> stdin_handler | config_input.File(reader) -> (reader.ReadLine)
        let _outp = match output with Stdout -> stdout_handler | config_output.File(writer) -> (writer.Write >> writer.Flush)
        io_handler(_inp, _outp)

// ### RUN PROGRAM USING CONFIG
// ### 
// ##  INTERACTIVE MODE
let rec interactive_handle_lines_from_stdin handler acc =
    printf "> "
    let line = Console.ReadLine()
    if line <> null 
        then interactive_handle_lines_from_stdin handler (handler acc line)
        else acc

let interactive_handler acc line =
    let _std_io_handler = create_io_handler Interactive
    match (run parser_stmt line) with
    | Failure(x, _, _) -> printf "Error parsing: %s" x; acc
    | Success(x, _, _) -> match (eval_stmt x acc _std_io_handler) with
                            | LetResult(x, y) -> Map.add x y acc
                            | NormalExprResult(x) -> printfn "%s" (format_data_obj x); acc

let private run_interactive () =
    interactive_handle_lines_from_stdin interactive_handler Map.empty

// ## SOURCE CODE MODE
let source_code_handler cfg =
    let src_file = match cfg with SourceCode(x, _, _) -> x | _ -> failwith "Internal logic error"
    let io = create_io_handler cfg
    let source_code = File.ReadAllText(src_file)
    match (run_main_parser source_code) with
        | parser_result.Success(x) -> run_stmt_list x io;
        | parser_result.Error(x) -> failwith x

let run_with_cfg cfg =
    match cfg with
      | Interactive -> run_interactive ()
      | SourceCode(_) -> source_code_handler cfg 