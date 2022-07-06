open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

(* I need to make alot of helpers. One per non-terminal *)
let rec parse_expr toks = 
  let (tokens, expression) = parse_expression toks in 
    tokens, expression

(* Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr *)
and parse_expression tokens = 
  match lookahead tokens with 
  | Some Tok_Let -> (* LetExpr *) 
    let (tokens_let, let_expression) = parse_let tokens in 
      tokens_let, let_expression 
  | Some Tok_If -> (* IfExpr *)
    let (tokens_If, if_expression) = parse_if tokens in
      tokens_If, if_expression
  | Some Tok_Fun -> (* FunctionExpr *)
     let (tokens_fun, fun_expression) = parse_function_expr tokens in
      tokens_fun, fun_expression
  | _ -> (* OrExpr *) 
    let (tokens_or, or_expression) = parse_or tokens in
      tokens_or, or_expression

(* LetExpr -> let Recursion Tok_ID = Expr in Expr ... where (Recursion -> rec | ε) *)
and parse_let tokens = 
  let tokens_no_let = match_token tokens Tok_Let in 
  let is_rec = (if lookahead tokens_no_let = Some Tok_Rec then
     true else false) in 
  let tokens_no_rec = (if is_rec = true then 
    match_token tokens_no_let Tok_Rec else tokens_no_let) in 
  match lookahead tokens_no_rec with 
  | Some Tok_ID id -> 
    let tokens_no_id = (match_token tokens_no_rec (Tok_ID id)) in 
    let tokens_no_equal = match_token tokens_no_id Tok_Equal in
    let (token_expression, expression) = parse_expression tokens_no_equal in
    let token_no_in = match_token token_expression Tok_In in
    let (token_2_expression, expression2) = parse_expression token_no_in in 
      token_2_expression, Let (id, is_rec, expression, expression2)
  | _ -> raise (InvalidInputException("parse_let (not an id)"))

(* FunctionExpr -> fun Tok_ID -> Expr *)
and parse_function_expr tokens = 
  let tokens_no_fun = match_token tokens Tok_Fun in 
  match lookahead tokens_no_fun with 
  | Some Tok_ID id -> 
    let tokens_no_id = (match_token tokens_no_fun (Tok_ID id)) in 
    let tokens_no_arrow = match_token tokens_no_id Tok_Arrow in
    let (token_expression, expression) = parse_expression tokens_no_arrow in
      token_expression, Fun (id, expression)
  | _ -> raise (InvalidInputException("parse_function_expr (not an id)"))

(* IfExpr -> if Expr then Expr else Expr *)
and parse_if tokens = 
  let tokens_no_if = match_token tokens Tok_If in
  let (token_expression, expression) = parse_expression tokens_no_if in 
  let tokens_no_then = match_token token_expression Tok_Then in 
  let (token_expression2, expression2) = parse_expression tokens_no_then in
  let tokens_no_else = match_token token_expression2 Tok_Else in 
  let (token_expression3, expression3) = parse_expression tokens_no_else in
    token_expression3, If (expression, expression2, expression3)

(* OrExpr -> AndExpr || OrExpr | AndExpr *)
and parse_or tokens = 
  let (tokens_and, expression_and) = parse_and tokens in
  if (lookahead tokens_and) = Some Tok_Or then 
    let tokens_no_or = match_token tokens_and Tok_Or in
    let (tokens_or, expression_or) = parse_or tokens_no_or in 
      tokens_or, Binop(Or, expression_and, expression_or)
  else 
    tokens_and, expression_and

(* EqualityExpr && AndExpr | EqualityExp *)
and parse_and tokens = 
  let (tokens_equality, expression_equality) = parse_equality tokens in 
  if  (lookahead tokens_equality) = Some Tok_And then
    let tokens_no_and = match_token tokens_equality Tok_And in
    let (tokens_and, expression_and) = parse_and tokens_no_and in 
      tokens_and, Binop(And, expression_equality, expression_and)
  else 
    tokens_equality, expression_equality

(* EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr *)
(* where EqualityOperator -> = or <> *)
and parse_equality tokens = 
  let (tokens_relational, expression_relational) = parse_relational tokens in 
  if (lookahead tokens_relational) = Some Tok_Equal then
    let tokens_no_equal = match_token tokens_relational Tok_Equal in
    let (tokens_equality, expression_equality) = parse_equality tokens_no_equal in
      tokens_equality, Binop(Equal, expression_relational, expression_equality)
  else if (lookahead tokens_relational) = Some Tok_NotEqual then 
    let tokens_no_notequal = match_token tokens_relational Tok_NotEqual in
    let (tokens_equality2, expression_equality2) = parse_equality tokens_no_notequal in
      tokens_equality2, Binop(NotEqual, expression_relational, expression_equality2)
  else 
    tokens_relational, expression_relational

(* RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr *)
(* Where RelationalOperator -> < | > | <= | >= *)
and parse_relational tokens =
  let (tokens_additive, expression_additive) = parse_additive tokens in 
  if (lookahead tokens_additive) = Some Tok_Less then
    let tokens_no_less = match_token tokens_additive Tok_Less in
    let (tokens_relational, expression_relational) = parse_relational tokens_no_less in
      tokens_relational, Binop(Less, expression_additive, expression_relational)
  else if (lookahead tokens_additive) = Some Tok_LessEqual then
    let tokens_no_less_equal = match_token tokens_additive Tok_LessEqual in
    let (tokens_relational2, expression_relational2) = parse_relational tokens_no_less_equal in
      tokens_relational2, Binop(LessEqual, expression_additive, expression_relational2)
  else if (lookahead tokens_additive) = Some Tok_Greater then
    let tokens_no_greater = match_token tokens_additive Tok_Greater in
    let (tokens_relational3, expression_relational3) = parse_relational tokens_no_greater in
      tokens_relational3, Binop(Greater, expression_additive, expression_relational3)
  else if (lookahead tokens_additive) = Some Tok_GreaterEqual then
    let tokens_no_greater_equal = match_token tokens_additive Tok_GreaterEqual in
    let (tokens_relational4, expression_relational4) = parse_relational tokens_no_greater_equal in
      tokens_relational4, Binop(GreaterEqual, expression_additive, expression_relational4)
  else 
    tokens_additive, expression_additive

(* AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr *)
(* Where AdditiveOperator -> + | - *)
and parse_additive tokens = 
  let (tokens_multiplicative, expression_multiplicative) = parse_multiplicative tokens in 
  if (lookahead tokens_multiplicative) = Some Tok_Add then
    let tokens_no_add = match_token tokens_multiplicative Tok_Add in
    let (tokens_additive, expression_additive) = parse_additive tokens_no_add in
      tokens_additive, Binop(Add, expression_multiplicative, expression_additive)
  else if (lookahead tokens_multiplicative) = Some Tok_Sub then 
    let tokens_no_sub = match_token tokens_multiplicative Tok_Sub in
    let (tokens_additive2, expression_additive2) = parse_additive tokens_no_sub in
      tokens_additive2, Binop(Sub, expression_multiplicative, expression_additive2)
  else 
    tokens_multiplicative, expression_multiplicative
   
(* MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr *)
(* MultiplicativeOperator -> * | / *)
and parse_multiplicative tokens = 
  let (tokens_concat, expression_concat) = parse_concat tokens in 
  if (lookahead tokens_concat) = Some Tok_Mult then
    let tokens_no_mult = match_token tokens_concat Tok_Mult in
    let (tokens_multiplicative, expression_multiplicative) = parse_multiplicative tokens_no_mult in 
      tokens_multiplicative, Binop(Mult, expression_concat, expression_multiplicative)
  else if (lookahead tokens_concat) = Some Tok_Div then
    let tokens_no_div = match_token tokens_concat Tok_Div in
    let (tokens_multiplicative2, expression_multiplicative2) = parse_multiplicative tokens_no_div in 
      tokens_multiplicative2, Binop(Div, expression_concat, expression_multiplicative2)
  else 
    tokens_concat, expression_concat

(* ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr *)
and parse_concat tokens = 
  let (tokens_unary, expression_unary) = parse_unary tokens in 
  if (lookahead tokens_unary) = Some Tok_Concat then
    let tokens_no_concat = match_token tokens_unary Tok_Concat in
    let (tokens_concat, expression_concat) = parse_concat tokens_no_concat in 
      tokens_concat, Binop(Concat, expression_unary, expression_concat)
  else 
    tokens_unary, expression_unary

(* UnaryExpr -> not UnaryExpr | FunctionCallExpr *)
and parse_unary tokens = 
  if (lookahead tokens) = Some Tok_Not then
    let tokens_no_not = match_token tokens Tok_Not in
    let (tokens_unary, expression_unary) = parse_unary tokens_no_not in 
      tokens_unary, Not (expression_unary)
  else 
    let (tokens_function_call, expression_function_call) = parse_function_call tokens in 
      tokens_function_call, expression_function_call

(* FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr *)
and parse_function_call tokens = 
  let (tokens_primary, expression_primary) = parse_primary tokens in 
  match lookahead tokens_primary with 
  | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen -> 
     let (tokens_primary2, expression_primary2) = parse_primary tokens_primary in
       tokens_primary2, FunctionCall (expression_primary, expression_primary2)
  | _ -> tokens_primary, expression_primary 
    

(* PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)
and parse_primary tokens = 
  match lookahead tokens with 
  | Some Tok_Int num -> 
    let tokens_no_int = (match_token tokens (Tok_Int num)) in
      tokens_no_int, Value (Int (num))
  | Some Tok_Bool bool -> 
    let tokens_no_bool = (match_token tokens (Tok_Bool bool)) in
      tokens_no_bool, Value (Bool (bool))
  | Some Tok_String str -> 
    let tokens_no_str = (match_token tokens (Tok_String str)) in
      tokens_no_str, Value (String (str))
  | Some Tok_ID id ->
    let tokens_no_id = (match_token tokens (Tok_ID id)) in
      tokens_no_id, ID (id)
  | Some Tok_LParen -> 
    let tokens_no_lparen = (match_token tokens Tok_LParen) in
    let (token_expression, expression) = parse_expression tokens_no_lparen in
    let tokens_no_rparen = (match_token token_expression Tok_RParen) in
      tokens_no_rparen, expression
  | _ -> raise (InvalidInputException("parse_primary (not an parimary expression)"))


(* Part 3: Parsing mutop *)

(* Mutop -> DefMutop | ExprMutop | ;; *)
let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_DoubleSemi -> 
    let tokens_no_double_semi = (match_token toks Tok_DoubleSemi) in
      tokens_no_double_semi, NoOp
  | Some Tok_Def -> 
    let (token_def, expression_def) = parse_def toks in
      token_def, expression_def
  | _ -> 
    let (token_expr_mutop, expression_expr_mutop) = parse_expr_mutop toks in
      token_expr_mutop, expression_expr_mutop

(* DefMutop -> def Tok_ID = Expr ;; *)
and parse_def toks = 
  let tokens_no_def = (match_token toks Tok_Def) in
  match lookahead tokens_no_def with 
  | Some Tok_ID id -> 
    let tokens_no_id = (match_token tokens_no_def (Tok_ID id)) in
    let tokens_no_equal = (match_token tokens_no_id Tok_Equal) in
    let (token_expression, expression) = parse_expression tokens_no_equal in
    let tokens_no_double_semi = (match_token token_expression Tok_DoubleSemi) in
      tokens_no_double_semi, Def (id, expression)
  | _ -> raise (InvalidInputException("parse_def (not an ID)"))

(* ExprMutop -> Expr ;; *)
and parse_expr_mutop toks = 
  let (token_expression, expression) = parse_expression toks in
  let tokens_no_double_semi = (match_token token_expression Tok_DoubleSemi) in
    tokens_no_double_semi, Expr (expression)