use "lexer.sml";

datatype expr =
    (* Literals *)
    IntLit of int            (* Integer literal, e.g., 42, -10 *)
  | BoolLit of bool 
  |CharLit of char           (* Boolean literal, e.g., True, False *)

    (* Variables and Identifiers *)
  | Var of string            (* A variable or function name, e.g., x, myFunc *)


    (* Binary Operators *)
  | BinOp of string * expr * expr (* Binary operation, e.g., x + y, a == b
                                     string: operator symbol (e.g., "+", "==")
                                     expr: left-hand side operand
                                     expr: right-hand side operand *)

    (* Conditional Expression *)
  | IfThenElse of expr * expr * expr (* If-then-else expression, e.g., if cond then e1 else e2
                                        expr: condition
                                        expr: 'then' branch
                                        expr: 'else' branch *)
  | StringLit of string (*  String literal *)

    (* Local Bindings (Let expression) *)
  | Let of (string * expr) list * expr

    
  | ParenExpr of expr        (* A parenthesized expression, e.g., (x + y) *)
  

datatype declaration =
    Decl of string * expr
  


(* ------------------------------------------------------------------------ *)
(* 2. Parser Helper Functions *)
(* ------------------------------------------------------------------------ *)

(*
 * `peek` function: Looks at the head of the token list without consuming it.
 * Returns `SOME token` if the list is not empty, `NONE` otherwise.
 *)
fun peek (tokens: token list) : token option =
    case tokens of
        (t :: _) => SOME t
      | [] => NONE

(*
 * `consume` function: Attempts to consume a specific token type from the head of the list.
 * If the head matches `expected_token_type`, returns `SOME remaining_tokens`.
 * Otherwise, returns `NONE`.
 *)
fun consume ((expected_token_type: token) , (tokens: token list)) : token list option =
    case tokens of
        (t :: ts) =>
            if t = expected_token_type then SOME ts
            else NONE
      | [] => NONE


(*
 * `consume_val` function: Attempts to consume a token and extract its associated value.
 * Takes a function `f` that, given a token, returns `SOME value` if the token is
 * of the desired type and has a value, or `NONE` otherwise.
 * If successful, returns `SOME (value, remaining_tokens)`.
 * Otherwise, returns `NONE`.
 *
 *)
fun consume_val ((f: token -> 'a option) , (tokens: token list) ): ('a * token list) option =
    case tokens of
        (t :: ts) =>
            (case f t of
                 SOME value => SOME (value, ts)
               | NONE => NONE)
               | [] => NONE



(*
 * Grammar: Expression ::= LetExpr | IfThenElseExpr | Term ( ('++' | '+' | '==' | '>' | '<') Term )*
 *)

fun parseBindings (tokens: token list) : ((string * expr) list * token list) option =
    case consume_val ((fn (TOK_IDENTIFIER s) => SOME s | _ => NONE) , tokens) of
        SOME (name, rest1) =>
            (case consume (TOK_EQUALS , rest1) of
                 SOME rest2 =>
                     (case parseExpression rest2 of (* Value for the binding *)
                          SOME (value_expr, rest3) =>
                              let
                                  val binding = (name, value_expr)
                                  fun collect_more_bindings current_tokens =
                                      (* Check if there's a semicolon, implying another binding *)
                                      case peek current_tokens of
                                          SOME TOK_SEMICOLON => (* NEW: Look for semicolon *)
                                              (case consume (TOK_SEMICOLON , current_tokens) of
                                                   SOME rest_after_semicolon =>
                                                       (case parseBindings rest_after_semicolon of (* Recursively parse more bindings *)
                                                            SOME (more_bindings, final_rest) => SOME (binding :: more_bindings, final_rest)
                                                          | NONE => NONE) (* Error in subsequent binding *)
                                                 | NONE => NONE) (* Should not happen if peek was correct *)
                                        | _ => SOME ([binding], current_tokens) (* No semicolon, this is the last binding *)
                              in
                                  collect_more_bindings rest3
                              end
                        | NONE => NONE) (* Error: Expected expression after '=' *)
               | NONE => NONE) (* Error: Expected '=' after identifier *)
      | NONE => NONE (* No identifier, so no binding *)


    
and  parseExpression (tokens: token list) : (expr * token list) option =
    (* Try to parse a Let expression first *)
    case consume( (TOK_KEYWORD "let") , tokens) of
        SOME rest_after_let =>
            (case parseBindings rest_after_let of (* <-- Calls parseBindings for multiple bindings *)
                 SOME (bindings, rest_after_bindings) =>
                     (case consume ((TOK_KEYWORD "in") , rest_after_bindings) of
                          SOME rest_after_in =>
                              (case parseExpression rest_after_in of
                                   SOME (in_body, rest_after_in_body) =>
                                       (case consume ((TOK_KEYWORD "end"),  rest_after_in_body) of
                                            SOME final_rest => SOME (Let (bindings, in_body), final_rest)
                                          | NONE => NONE) (* Error: Expected 'end' *)
                                 | NONE => NONE) (* Error: Expected body after 'in' *)
                           | NONE => NONE) (* Error: Expected 'in' *)
                 | NONE => NONE) (* Error: Expected bindings after 'let' *)
    (* If not a Let expression, try to parse an If-Then-Else expression *)
    | NONE =>
   (case consume ((TOK_KEYWORD "if") ,tokens) of
         SOME rest_after_if_kw =>
             (case parseExpression rest_after_if_kw of
                  SOME (cond, rest_after_cond) =>
                      (case consume ((TOK_KEYWORD "then") , rest_after_cond) of
                           SOME rest_after_then_kw =>
                               (case parseExpression rest_after_then_kw of
                                    SOME (t_branch, rest_after_t_branch) =>
                                        (case consume ((TOK_KEYWORD "else") ,rest_after_t_branch) of
                                             SOME rest_after_else_kw =>
                                                 (case parseExpression rest_after_else_kw of
                                                      SOME (e_branch, rest_after_e_branch) => SOME (IfThenElse (cond, t_branch, e_branch), rest_after_e_branch)
                                                    | NONE => NONE) (* Error: Expected else branch *)
                                               | NONE => NONE) (* Error: Expected 'else' *)
                                      | NONE => NONE) (* Error: Expected then branch *)
                            | NONE => NONE) (* Error: Expected 'then' *)
                      | NONE => NONE) (* Error: Expected condition after 'if' *)
         | NONE => (* If not 'if', then parse a standard expression starting with a Term *)
         (case parseTerm tokens of
                  SOME (first_term, rest_after_first_term) =>
                      let
                          (* Helper to parse subsequent binary operations (+, ==, >, <) *)
                          fun parse_more_binops current_expr remaining_tokens =
                              case peek remaining_tokens of
                                  SOME (TOK_OPERATOR "+") => (* Directly match TOK_OPERATOR "+" *)
                                      (case consume ((TOK_OPERATOR "+"), remaining_tokens) of
                                           SOME rest_after_op =>
                                               (case parseTerm rest_after_op of
                                                    SOME (next_term, rest_after_next_term) =>
                                                        parse_more_binops (BinOp ("+", current_expr, next_term)) rest_after_next_term
                                                  | NONE => NONE)
                                         | NONE => NONE)
                                  |SOME (TOK_OPERATOR "++") => (* Directly match TOK_OPERATOR "+" *)
                                      (case consume ((TOK_OPERATOR "++"), remaining_tokens) of
                                           SOME rest_after_op =>
                                               (case parseTerm rest_after_op of
                                                    SOME (next_term, rest_after_next_term) =>
                                                        parse_more_binops (BinOp ("++", current_expr, next_term)) rest_after_next_term
                                                  | NONE => NONE)
                                         | NONE => NONE)

                                  |SOME (TOK_OPERATOR "==") => (* Directly match TOK_OPERATOR "==" *)
                                        (case consume ((TOK_OPERATOR "==") ,remaining_tokens) of
                                           SOME rest_after_op =>
                                               (case parseTerm rest_after_op of
                                                    SOME (next_term, rest_after_next_term) =>
                                                        parse_more_binops (BinOp ("==", current_expr, next_term)) rest_after_next_term
                                                  | NONE => NONE)
                                         | NONE => NONE)
                                  |SOME (TOK_OPERATOR ">") => (* Directly match TOK_OPERATOR ">" *)
                                        (case consume ((TOK_OPERATOR ">") ,remaining_tokens) of
                                           SOME rest_after_op =>
                                               (case parseTerm rest_after_op of
                                                    SOME (next_term, rest_after_next_term) =>
                                                        parse_more_binops (BinOp (">", current_expr, next_term)) rest_after_next_term
                                                  | NONE => NONE)
                                         | NONE => NONE)
                                  | SOME (TOK_OPERATOR "<") => (* Directly match TOK_OPERATOR "<" *)
                                         (case consume ((TOK_OPERATOR "<"), remaining_tokens) of
                                           SOME rest_after_op =>
                                               (case parseTerm rest_after_op of
                                                    SOME (next_term, rest_after_next_term) =>
                                                        parse_more_binops (BinOp ("<", current_expr, next_term)) rest_after_next_term
                                                  | NONE => NONE)
                                  | NONE => NONE) 
                                        
                                        
                                | _ => SOME (current_expr, remaining_tokens) (* No more operators *)

                      in
                          parse_more_binops first_term rest_after_first_term
                      end
                    | NONE => (print "DEBUG: parseExpression - parseTerm failed.\n"; NONE))) (* DEBUG *)
    
    




and parseFactor (tokens: token list) : (expr * token list) option =
    (case tokens of
        (TOK_INT n :: rest) => SOME (IntLit n, rest)
      | (TOK_KEYWORD "True" :: rest) => SOME (BoolLit true, rest)
      | (TOK_KEYWORD "False" :: rest) => SOME (BoolLit false, rest)
      | (TOK_IDENTIFIER s :: rest) => SOME (Var s, rest) 
      | (TOK_STRING s :: rest) => SOME (StringLit s, rest) 
        | (TOK_CHAR c :: rest) => SOME (CharLit c,rest )   
      | (TOK_LPAREN :: rest_after_lparen) =>
            (case parseExpression rest_after_lparen of
                 SOME (expr_val, rest_after_expr) =>
                     (case consume (TOK_RPAREN, rest_after_expr) of
                          SOME rest_after_rparen => SOME (ParenExpr expr_val, rest_after_rparen)
                        | NONE => NONE)
               | NONE => NONE)
      | _ => NONE)
     (* This semicolon terminates the entire fun ... and ... block *)


and parseTerm tokens =
    (case parseFactor tokens of
        SOME (first_factor, rest_after_first_factor) =>
            let
                (* Helper to parse subsequent multiplications *)
                fun parse_more_term_ops current_expr remaining_tokens =
                    case peek remaining_tokens of
                        SOME (TOK_OPERATOR "*") =>
                            (case consume ((TOK_OPERATOR "*") ,remaining_tokens) of
                                 SOME rest_after_mul_op =>
                                     (case parseFactor rest_after_mul_op of
                                          SOME (next_factor, rest_after_next_factor) =>
                                              parse_more_term_ops (BinOp ("*", current_expr, next_factor)) rest_after_next_factor
                                        | NONE => NONE) (* Error: Expected Factor after '*' *)
                               | NONE => NONE) (* Should not happen if peek was correct *)
                      | _ => SOME (current_expr, remaining_tokens) (* No more multiplications *)
            in
                parse_more_term_ops first_factor rest_after_first_factor
            end
      | NONE => NONE) (* Error: Failed to parse first Factor *)



(* NEW: parseDeclaration function *)
fun parseDeclaration (tokens: token list) : (declaration * token list) option =
    (case consume_val ((fn (TOK_IDENTIFIER s) => SOME s | _ => NONE) , tokens) of
         SOME (name, rest1) =>
             (case consume (TOK_EQUALS , rest1) of
                  SOME rest2 =>
                      (case parseExpression rest2 of
                           SOME (expr_val, rest3) => SOME (Decl (name, expr_val), rest3)
                         | NONE => NONE) (* Error: Expected expression after '=' *)
                | NONE => NONE) (* Error: Expected '=' after identifier *)
       | NONE => NONE) (* Not an identifier, so not a declaration *)
    ;
  

fun parseProgram (tokens: token list) : (declaration list * token list) option =
    let
        fun collect_declarations current_tokens acc_decls =
            case peek current_tokens of
                SOME TOK_EOF => SOME (List.rev acc_decls, current_tokens) (* Reached EOF *)
              | _ =>
                  (case parseDeclaration current_tokens of
                       SOME (decl, rest_after_decl) =>
                           (* After parsing a declaration, try to consume a semicolon *)
                           (case consume (TOK_SEMICOLON , rest_after_decl) of
                                SOME rest_after_semicolon =>
                                    (* If semicolon found, continue collecting with the rest *)
                                    collect_declarations rest_after_semicolon (decl :: acc_decls)
                              | NONE =>
                                    (* If no semicolon, check if it's EOF or if another declaration immediately follows *)
                                    (* This allows the last declaration to not require a semicolon *)
                                    (case peek rest_after_decl of
                                         SOME TOK_EOF => SOME (List.rev (decl :: acc_decls), rest_after_decl) (* Last declaration before EOF *)
                                       | _ => NONE) (* Error: Expected semicolon or EOF after declaration *)
                           )
                     | NONE => NONE) (* If we expect a declaration but can't parse one, it's an error *)
    in
        collect_declarations tokens []
    end
    ;



(*
 * parseTerm: Handles multiplication.
 * Grammar: Term ::= Factor ( '*' Factor )*
 * This implements left-associativity for multiplication.
 *)



















(*
 * `parse` function:
 * Takes the raw input string, tokenizes it using the lexer,
 * and then attempts to parse the resulting token list into an AST.
 * Returns `SOME expr` if successful and all tokens (except EOF) are consumed,
 * otherwise returns `NONE`.
 *)
fun parse (input_string: string)  =
    let
        val tokens = tokenize input_string (* Assumes 'tokenize' from lexer is available *)
    in
        case parseProgram  tokens of
            SOME (decls, [TOK_EOF]) => SOME decls (* Successfully parsed and consumed all tokens including EOF *)
          | SOME (_, _) => NONE (* Parsed something, but didn't consume all tokens (e.g., leftover tokens) *)
          | NONE => NONE (* Failed to parse anything *)
    end;

(* ------------------------------------------------------------------------ *)
(* 5. AST Pretty-Printer (for verification) *)
(* ------------------------------------------------------------------------ *)

(* Helper function to convert an AST node back into a readable string *)
fun print_expr ast =
    case ast of
        IntLit n => Int.toString n
      | BoolLit b => if b then "True" else "False"
      | Var s => s
      (*| App (f, x) => "(" ^ (print_expr f) ^ " " ^ (print_expr x) ^ ")"*)
      | BinOp (s, e1, e2) => "(" ^ (print_expr e1) ^ " " ^ s ^ " " ^ (print_expr e2) ^ ")" 
      | StringLit s => "\"" ^ s ^ "\"" (* NEW: Print string literals *)
      | CharLit c => "'" ^ Char.toString c ^ "'"
      | IfThenElse (cond, t, e) => "(if " ^ (print_expr cond) ^ " then " ^ (print_expr t) ^ " else " ^ (print_expr e) ^ ")"
      | ParenExpr e => "(" ^ (print_expr e) ^ ")"
      | Let (bindings, body) =>
            let
                fun print_binding (name, expr) = name ^ " = " ^ (print_expr expr)
                val binding_strs = List.map print_binding bindings
            in
                "(let " ^ (String.concatWith "; " binding_strs) ^ " in " ^ (print_expr body) ^ " end)"
            end
    ;


fun print_decl (Decl (name, expr_val)) =
    name ^ " = " ^ (print_expr expr_val)
    ;


