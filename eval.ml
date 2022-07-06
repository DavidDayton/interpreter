open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Value value -> value

  | ID id -> lookup env id 

  | Not unary_expression -> 
    (match unary_expression with 
    | Not unary_expression -> eval_expr env unary_expression
    | ID id -> let id_value = lookup env id in 
      (match id_value with 
      | Bool bool -> if (bool = true) then Bool (false) else Bool (true)
      | _ -> raise (TypeError("Expected Type Bool from id")))
    | Value value -> 
      (match value with 
      | Bool bool -> if (bool = true) then Bool (false) else Bool (true)
      | _ -> raise (TypeError("Expected Type Bool from value")))
    | _ -> raise (TypeError("Expected Type Bool")))

  | Binop (op, expr1, expr2) -> 
    (match expr1, expr2 with 
    | ((Binop _ | Value _ | ID _ | FunctionCall _ | If _ | Not _), (Binop _ | Value _ | ID _ | FunctionCall _ | If _ | Not _)) -> 
      let value_expr1 = eval_expr env expr1 in 
      let value_expr2 = eval_expr env expr2 in 
      (match op with 
      | Add -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) -> 
          Int (num1 + num2)
        | _ -> raise (TypeError("Add expected Type Int")))
      | Sub -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) ->  
          Int (num1 - num2)
        | _ -> raise (TypeError("Sub expected Type Int")))
      | Mult -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) -> 
          Int (num1 * num2)
        | _ -> raise (TypeError("Mult expected Type Int")))
      | Div -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) ->  
          if num2 = 0 then raise (DivByZeroError) else Int (num1 / num2)
        | _ -> raise (TypeError("Div expected Type Int")))
      | Greater -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) ->  
          if num1 > num2 then Bool true else Bool false
        | _ -> raise (TypeError("Greater expected Type Int")))
      | Less -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) ->  
          if num1 < num2 then Bool true else Bool false
        | _ -> raise (TypeError("Less expected Type Int")))
      | GreaterEqual -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) ->  
          if num1 >= num2 then Bool true else Bool false
        | _ -> raise (TypeError("GreaterEqual expected Type Int")))
      | LessEqual -> 
        (match value_expr1, value_expr2 with 
        | (Int num1, Int num2) ->  
          if num1 <= num2 then Bool true else Bool false
        | _ -> raise (TypeError("LessEqual expected Type Int")))
      | Concat -> 
        (match value_expr1, value_expr2 with 
        | (String str1, String str2) ->  
          String (str1 ^ str2)
        | _ -> raise (TypeError("Concat expected Type String")))
      | Equal -> 
        (match value_expr1, value_expr2 with 
        | (String str1, String str2) ->  
          if str1 = str2 then Bool true else Bool false
        | (Int num1, Int num2) ->  
          if num1 = num2 then Bool true else Bool false
        | (Bool bool1, Bool bool2) -> 
          if bool1 = bool2 then Bool true else Bool false
        | _ -> raise (TypeError("Equal Binop type error")))
      | NotEqual -> 
        (match value_expr1, value_expr2 with 
        | (String str1, String str2) ->  
          if str1 <> str2 then Bool true else Bool false
        | (Int num1, Int num2) ->  
          if num1 <> num2 then Bool true else Bool false
        | (Bool bool1, Bool bool2) -> 
          if bool1 <> bool2 then Bool true else Bool false
        | _ -> raise (TypeError("Equal Binop type error")))
      | Or -> 
        (match value_expr1, value_expr2 with 
        | (Bool bool1, Bool bool2) -> 
          Bool (bool1 || bool2)
        | _ -> raise (TypeError("Or expected Type Bool")))
      | And -> 
        (match value_expr1, value_expr2 with 
        | (Bool bool1, Bool bool2) -> 
          Bool (bool1 && bool2)
        | _ -> raise (TypeError("Or expected Type Bool")))
      (* | _ -> raise (TypeError("Expected An OP"))*) )
    | _ -> raise (TypeError("Binop Expected Type Value from expr1 or 2")))

  | If (expr1, expr2, expr3) -> 
    let value_expr1 = eval_expr env expr1 in 
    (match value_expr1 with 
    | Bool bool -> 
      if bool = true then eval_expr env expr2 else eval_expr env expr3
    | _ -> raise (TypeError("If Gaurd Expected Type Bool")))

  | Let (name_var, rec_bool, expr1, expr2) ->
    if rec_bool = false then (* Non-recursive *)
      let value_expr1 = eval_expr env expr1 in 
      let new_env = extend env name_var value_expr1 in (* Add the value of expr1 to the env with the name name_var *)
      eval_expr new_env expr2
    else (* Recursive *)
      let new_env_temp = extend_tmp env name_var in
      let value_expr1 = eval_expr new_env_temp expr1 in
      update new_env_temp name_var value_expr1;
      (* Closure(new_env_temp, "TEST", Value(Int (-1))) *)
      eval_expr new_env_temp expr2 

  | Fun (name_var, expr1) -> Closure(env, name_var, expr1)

  | FunctionCall (expr1, expr2) -> 
    let value_expr2 = eval_expr env expr2 in 
    let value_expr1 = eval_expr env expr1 in 
    match value_expr1 with 
    | Closure (cl_env, name_var, cl_expr) ->  
      let new_env = extend cl_env name_var value_expr2 in
      (* Closure(new_env, "TEST", cl_expr) *)
      eval_expr new_env cl_expr
    | _ ->  raise (TypeError("Not a closure (function call)"))

  (* | _ -> raise (TypeError("Not an environment")) *)
  


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | Def (var, expr1) -> 
    let new_env_temp = extend_tmp env var in
    let value_expr1 = eval_expr new_env_temp expr1 in
    update new_env_temp var value_expr1;
    (new_env_temp, Some value_expr1)
  | Expr expr1 -> 
    let value_expr1 = eval_expr env expr1 in
    (env, Some (value_expr1))
  | NoOp -> [], None


