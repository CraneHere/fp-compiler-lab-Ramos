module Core

open Types

open System.IO

// Operator apply
let private operator_apply_string_to_func_int op =
    let cmp_wrapper f y =
        match (f y) with
        | true -> 1
        | false -> 0
    match op with
    | "+" -> (+)
    | "-" -> (-)
    | "*" -> (*)
    | "/" -> (/)
    | "<" -> (<) >> cmp_wrapper
    | ">" -> (>) >> cmp_wrapper
    | "=" -> (=) >> cmp_wrapper
    | "<=" -> (<=) >> cmp_wrapper
    | ">=" -> (>=) >> cmp_wrapper
    | "!=" -> (<>) >> cmp_wrapper
    | _ -> failwith "Error operator_apply_string_to_func_int: Bad operator"

let rec private operator_apply op obj1 obj2 =
    let op_func_int = operator_apply_string_to_func_int op in
    match (op, obj1, obj2) with
    | (_, Int(x), Int(y)) -> Int(op_func_int x y)
    | ("+", String(x), String(y)) -> String(x + y)
    | ("+", Int(x), String(y)) -> String(x.ToString() + y)
    | ("+", String(x), Int(y)) -> String(x + y.ToString())
    | ("=", String(x), String(y)) -> if (x = y) then Int(1) else Int(0)
    
    | ("=", Null, Null) -> Int(1)
    | ("=", Null, _) -> Int(0)
    | ("=", _, Null) -> Int(0)

    | ("=", Undefined, Undefined) -> Int(1)
    | ("=", Undefined, _) -> Int(0)
    | ("=", _, Undefined) -> Int(0)

    | ("!=", x, y) -> match (operator_apply "=" x y) with Int(x) -> Int(1 - x) | _ -> failwith (sprintf "Error operator_apply: Error applying %s to %A and %A" op obj1 obj2)
    
    | _ -> failwith (sprintf "Error operator_apply: Error applying %s to %A and %A" op obj1 obj2)

let rec private resolve_var_in_object var obj =
    match var with
    | [] -> obj
    | h::t -> match obj with
                | ComplexObject(table) -> if (Map.containsKey h table)
                                            then resolve_var_in_object t (Map.find h table)
                                            else data_object.Undefined
                | _ -> data_object.Undefined

let private resolve_var_in_env var env =
    if (Map.containsKey (List.head var) env) 
        then resolve_var_in_object (List.tail var) (Map.find (List.head var) env)
    else data_object.Undefined

let format_data_obj obj =
    let INDENT_STEP = 2 in
    let rec _format_data_obj_with_indent obj indent = 
        let indentation = String.replicate indent " " in
            match obj with
            | Int(x) -> sprintf "%d" x
            | String(x) -> sprintf "%s" x
            | Null -> sprintf "$null"
            | Undefined -> sprintf "$undefined"
            | ComplexObject(table) -> sprintf "{\n" + (Map.fold (fun st k v -> st + indentation + (String.replicate INDENT_STEP " ") + (sprintf "%s : " k) + (_format_data_obj_with_indent v (indent + INDENT_STEP) ) + "\n" ) "" table) + indentation + "}"
            | Function(_) -> sprintf "$function"
            | Reader(_) -> sprintf "$reader"
            | Writer(_) -> sprintf "$writer"
     in
     _format_data_obj_with_indent obj 0

// %$%$%$%$ Most important function
let rec eval expr env io =
    match expr with
    | DataObject(obj) -> obj
    | Operator(op, e1, e2) -> operator_apply op (eval e1 env io) (eval e2 env io)
    | UnresolvedComplexObject(table) -> ComplexObject(_resolve_c_obj_table table env io)
    | Var(lst) -> resolve_var_in_env lst env

    | Print(lst) -> List.fold (fun st x -> (io_w io) (format_data_obj (eval x env io)); st) None lst |>ignore ; Null
    | Println(lst) -> List.fold (fun st x -> (io_w io) (format_data_obj (eval x env io)); st) None lst |> ignore; (io_w io "\n"); Null
    
    | ReadString -> String((io_r io) () )
    | ReadInt -> let st = ((io_r) io () ) in Int(int(st))
    | ToInt(x) -> match (eval x env io) with
                    | Int(_) as i -> i
                    | String(s) -> Int(int(s))
                    | _ as obj -> failwith (sprintf "Can't convert to int %s" (format_data_obj obj))

    | If(cond, texp, fexp) -> match (eval cond env io) with
                                | Int(0) -> eval fexp env io
                                | _ -> eval texp env io

    | Lambda(arg, body) -> Function(arg, body, env)
    | Apply(DataObject(Function(arg, body, closure_env)), exp_lst) -> let evaled_exp_lst = List.map (fun x -> eval x env io) exp_lst in
                                                                            (_apply_func (Function(arg, body, closure_env)) evaled_exp_lst env io)
    | Apply(exp, exp_lst) -> eval (Apply(DataObject(eval exp env io), exp_lst)) env io

    | LocalLet(id, exp, body) -> eval body (Map.add id (eval exp env io) env) io

    // Queries:
    | Query(e, "undef", _) -> match (eval e env io) with Undefined -> Int(1) | _ -> Int(0)
    | Query(e, "def", _) -> match (eval e env io) with Undefined -> Int(0) | _ -> Int(1)
    | Query(e, "null", _) -> match (eval e env io) with Null -> Int(1) | _ -> Int(0)
    | Query(e, "int", _) -> match (eval e env io) with Int(_) -> Int(1) | _ -> Int(0)
    | Query(e, "function", _) -> match (eval e env io) with Function(_) -> Int(1) | _ -> Int(0)
    | Query(e, "string", _) -> match (eval e env io) with String(_) -> Int(1) | _ -> Int(0)
    | Query(e, "com_obj", _) -> match (eval e env io) with ComplexObject(_) -> Int(1) | _ -> Int(0)

    // File readers & writers
    | CreateFileReader(ex) -> match (eval ex env io) with 
                                | String(name) -> let r = new StreamReader(name) in Reader(r.ReadLine)
                                | _ as obj -> failwith (sprintf "Can't create file_reader from %s" (format_data_obj obj))
    | CreateFileWriter(ex) -> match (eval ex env io) with 
                                | String(name) -> let w = new StreamWriter(name) in Writer(w.Write >> w.Flush)
                                | _ as obj -> failwith (sprintf "Can't create file_writer from %s" (format_data_obj obj))
    | Read(ex) -> match (eval ex env io) with
                    | Reader(_r) -> String(_r ())
                    | _ as obj -> failwith (sprintf "Can't read from %s" (format_data_obj obj))
    | Write(what, where) -> match (eval where env io) with
                             | Writer(_w) -> let obj = (eval what env io) in _w (format_data_obj obj); Null
                             | _ as obj -> failwith (sprintf "Can't write from %s" (format_data_obj obj)) 
    | _ -> failwith (sprintf "Error eval: %A" expr)
and _resolve_c_obj_table table env io =
    Map.fold (fun st k v -> Map.add k (eval v env io) st) Map.empty table
and _apply_func func arg_list env io =
    if List.isEmpty arg_list then func
    else match func with 
            | Function(arg, body, closure_env) -> let current_value = (List.head arg_list) in
                                                    let current_step = eval body (Map.add arg current_value (Map.fold (fun st k v -> Map.add k v st) env closure_env)) io in
                                                        _apply_func current_step (List.tail arg_list) env io
            | _ -> failwith (sprintf "Error _apply_func: can't apply %s" (format_data_obj func))

// %$%$%$%$ Second most important function
let eval_stmt stmt env io =
    match stmt with
    | Let(name, vars, body) -> if (List.isEmpty vars)
                                 then LetResult(name, eval body env io)
                                 else let nested_func = List.foldBack (fun arg exp -> Lambda(arg, exp)) vars body in
                                        LetResult(name, eval nested_func env io)
    | NormalExpr(body) -> NormalExprResult(eval body env io)
    | _ -> failwith (sprintf "Error eval_stmt: error with stmt %A" stmt)

let run_stmt_list stmt_list io =
    let process_stmt env stmt =
        let stmt_res = eval_stmt stmt env io in
            match stmt_res with
              | LetResult(name, value) -> Map.add name value env
              | _ -> env
    List.fold process_stmt Map.empty stmt_list
