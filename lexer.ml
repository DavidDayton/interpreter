open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_WhiteSpace = Str.regexp "[ \n\t]+"
let re_NumPos = Str.regexp "[0-9]+" 
let re_NumNeg = Str.regexp "(-[0-9]+)"
let re_Bool = Str.regexp "true\\|false"
let re_String = Str.regexp "\"[^\"]*\""
let re_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_LParen = Str.regexp "("
let re_RParen = Str.regexp ")"
let re_Equal = Str.regexp "="
let re_NotEqual = Str.regexp "<>"
let re_Greater = Str.regexp ">"
let re_Less = Str.regexp "<"
let re_GreaterEqual = Str.regexp ">=" 
let re_LessEqual = Str.regexp "<="
let re_Or = Str.regexp "||"
let re_And = Str.regexp "&&" 
let re_Not = Str.regexp "not"
let re_If = Str.regexp "if"
let re_Then = Str.regexp "then"
let re_Else = Str.regexp "else"
let re_Add = Str.regexp "+"
let re_Sub = Str.regexp "-"
let re_Mult = Str.regexp "*"
let re_Div = Str.regexp "/"
let re_Concat = Str.regexp "\\^"
let re_Let = Str.regexp "let" 
let re_Def = Str.regexp "def"
let re_In = Str.regexp "in"
let re_Rec = Str.regexp "rec"
let re_Fun = Str.regexp "fun"
let re_Arrow = Str.regexp "->"
let re_DoubleSemi = Str.regexp ";;"

(* Determines if the ID is actually an ID *)
let id_helper string = 
    if (String.compare string "let" == 0) then 
        Tok_Let
    else if (String.compare string "not" == 0) then
        Tok_Not
    else if (String.compare string "if" == 0) then
        Tok_If
    else if (String.compare string "then" == 0) then
        Tok_Then
    else if (String.compare string "else" == 0) then
        Tok_Else
    else if (String.compare string "def" == 0) then
        Tok_Def
    else if (String.compare string "rec" == 0) then
        Tok_Rec
    else if (String.compare string "fun" == 0) then
        Tok_Fun
    else if (String.compare string "in" == 0) then
        Tok_In
    else (Tok_ID string)
;;

let rec tokanize_helper position string token_list = 
    if position >= (String.length string) then 
        token_list
    else  
        if (Str.string_match re_WhiteSpace string position) then (* White Space (" ", \n, and \t) *)
            let white_space_string = Str.matched_string string in
                tokanize_helper (position + (String.length white_space_string)) string token_list
        else if (Str.string_match re_NumPos string position) then (* Positive Num *)
            let num_string = Str.matched_string string in 
            let num_pos = int_of_string num_string in 
                tokanize_helper (position + (String.length num_string)) string (token_list @ [Tok_Int num_pos])
        else if (Str.string_match re_NumNeg string position) then (* Negative Num *)
            let num_neg_string = Str.matched_string string in 
            let num_neg = int_of_string (String.sub num_neg_string 2 ((String.length num_neg_string) - 3)) in
                tokanize_helper (position + (String.length num_neg_string)) string (token_list @ [Tok_Int (-num_neg)])
        else if (Str.string_match re_Bool string position) then (* bool "true" and "false" *)
            let bool_string = Str.matched_string string in
            let the_bool = if (bool_string = "true") then true else false in 
                 tokanize_helper (position + (String.length bool_string)) string (token_list @ [Tok_Bool the_bool]) 
        else if (Str.string_match re_String string position) then (* Strings (surronded by quotes) *)
            let string_with_quotes = Str.matched_string string in
            let string_without_quotes = String.sub string_with_quotes 1 ((String.length string_with_quotes) - 2) in
                tokanize_helper (position + (String.length string_with_quotes)) string (token_list @ [Tok_String string_without_quotes])  
        else if (Str.string_match re_ID string position) then (* ID (any letter followed by 0 or more letters or #s) *)
            let id_string = Str.matched_string string in
                tokanize_helper (position + (String.length id_string)) string (token_list @ [(id_helper id_string)])  
        else if (Str.string_match re_LParen string position) then (* Left Paren "(" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_LParen])
        else if (Str.string_match re_RParen string position) then (* Right Paren ")" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_RParen])
        else if (Str.string_match re_Arrow string position) then (* Arrow "->" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_Arrow])
        else if (Str.string_match re_Equal string position) then (* Equal "=" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Equal])
        else if (Str.string_match re_NotEqual string position) then (* NotEqual "<>" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_NotEqual])
        else if (Str.string_match re_GreaterEqual string position) then (* Greater than or = to ">=" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_GreaterEqual])
        else if (Str.string_match re_LessEqual string position) then (* Less than or = to "<=" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_LessEqual])    
        else if (Str.string_match re_Greater string position) then (* Greater than ">" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Greater])
        else if (Str.string_match re_Less string position) then (* Less than "<" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Less])
        else if (Str.string_match re_Or string position) then (* or "||" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_Or])
        else if (Str.string_match re_And string position) then (* and "&&" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_And])
        else if (Str.string_match re_Not string position) then (* not "not" *)
            tokanize_helper (position + 3) (string) (token_list @ [Tok_Not])
        else if (Str.string_match re_If string position) then (* if "if" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_If])
        else if (Str.string_match re_Then string position) then (* then "then" *)
            tokanize_helper (position + 4) (string) (token_list @ [Tok_Then])
        else if (Str.string_match re_Else string position) then (* else "else" *)
            tokanize_helper (position + 4) (string) (token_list @ [Tok_Else])
        else if (Str.string_match re_Add string position) then (* add "+" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Add])
        else if (Str.string_match re_Sub string position) then (* Subtraction "-" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Sub])
        else if (Str.string_match re_Mult string position) then (* Multiply "*" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Mult])
        else if (Str.string_match re_Div string position) then (* divide "/" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Div])
        else if (Str.string_match re_Concat string position) then (* Concatination "^" *)
            tokanize_helper (position + 1) (string) (token_list @ [Tok_Concat])
        else if (Str.string_match re_Let string position) then (* let "let" *)
            tokanize_helper (position + 3) (string) (token_list @ [Tok_Let])
        else if (Str.string_match re_Def string position) then (* def "def" *)
            tokanize_helper (position + 3) (string) (token_list @ [Tok_Def])
        else if (Str.string_match re_In string position) then (* in "in" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_In])
        else if (Str.string_match re_Rec string position) then (* rec "rec" *)
            tokanize_helper (position + 3) (string) (token_list @ [Tok_Rec])
        else if (Str.string_match re_Fun string position) then (* fun "fun" *)
            tokanize_helper (position + 3) (string) (token_list @ [Tok_Fun])
        else if (Str.string_match re_DoubleSemi string position) then (* ;; ";;" *)
            tokanize_helper (position + 2) (string) (token_list @ [Tok_DoubleSemi])
        else raise (InvalidInputException(string))
;;     

let tokenize input = 
    tokanize_helper 0 input []
;;
    

