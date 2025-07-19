use "lexer.sml";
use "parser.sml";
use "interpreter.sml";



val global_env_ref:env ref = ref (EnvList []);

(* In your main.sml *)

fun processInput (input_str: string) : unit =
     let
         val tokens = tokenize input_str; (* Get tokens from lexer *)
         val () = print ("  Tokens from lexer: " ^ (tokenListToString tokens) ^ "\n");
         val current_global_env = !global_env_ref;
     in
         case parseProgram tokens of
              SOME (declarations, [TOK_EOF]) =>
                   let
                       (* 1. Create a mutable reference for the environment that will contain
                          the new, mutually recursive top-level declarations. Initialize it to an empty EnvList.
                          This reference will be updated after the thunks are created. *)
                       val top_level_env_ref : env ref = ref (EnvList []);

                       (* 2. Create the VThunks for each declaration. Each thunk's environment
                          is set to `!top_level_env_ref`. This is the "tying the knot" step:
                          the thunks implicitly refer to the environment that is *being built*. *)
                       val lazy_top_level_bindings =
                           List.map (fn (Decl (name, expr_val)) =>
                               (name, VThunk (expr_val, top_level_env_ref, ref NONE)))
                               declarations;

                       (* 3. Construct the full new top-level environment.
                          This involves prepending the lazy bindings to the previous global environment. *)
                       val (EnvList prev_global_env_list) = current_global_env;
                       val final_top_level_env_list = lazy_top_level_bindings @ prev_global_env_list;

                       (* 4. Update the mutable reference `top_level_env_ref` to point to this
                          newly constructed environment. Now, all `VThunk`s created in step 2
                          correctly point to this complete, self-referential environment, enabling mutual recursion. *)
                       val () = top_level_env_ref := EnvList final_top_level_env_list

                   in
                       global_env_ref := !top_level_env_ref; (* Update the main global_env_ref *)
                       print ("  Global Environment updated after program: " ^ (String.concatWith ", " (List.map (fn (n, v) => n ^ " = " ^ (valueToString (forceValue v))) (case !global_env_ref of EnvList l => l)) ^ "\n"))
                   end

              (* If not a complete Program, try parsing as a SINGLE DECLARATION *)
            | _ =>
                (case parseDeclaration tokens of
                     SOME (Decl (name, expr_val), [TOK_EOF]) =>
                         (print ("  Parsed as Single Declaration: " ^ (print_decl (Decl (name, expr_val))) ^ "\n");
                          let
                              val (EnvList current_env_list) = current_global_env;
                              val new_value = VThunk (expr_val, global_env_ref, ref NONE); (* Pass global_env_ref directly *)
                              val new_env_list = EnvList ((name, new_value) :: current_env_list);
                          in
                              global_env_ref := new_env_list;
                              print ("    Stored as (unforced): " ^ (valueToString new_value) ^ "\n");
                              print ("  Global Environment updated.\n")
                          end
                          
                        )
                    (* If not a Declaration, try parsing as an EXPRESSION *)
                      | _ =>
                          (case parseExpression tokens of
                               SOME (ast, [TOK_EOF]) =>
                                   (print ("  Parsed as Expression: " ^ (print_expr ast) ^ "\n");
                                    let
                                        (* Evaluate the expression. This will return a value (potentially a VThunk). *)
                                        val result_val_unforced = eval_expr (ast, global_env_ref);
                                        (* Force the result to its concrete value. forceValue handles the memoization internally. *)
                                        val result_val_forced = forceValue result_val_unforced;
                                    in
                                        print ("  Evaluated Result (forced): " ^ (valueToString result_val_forced) ^ "\n")
                                    end)
                               (* If neither a Program, Declaration, nor a complete Expression *)
                               | SOME (_, remaining_tokens) => print ("  Parsing failed: Unconsumed tokens remaining: " ^ (tokenListToString remaining_tokens) ^ "\n")
                               | NONE => print "  Parsing failed: Input is neither a valid program, declaration, nor a complete expression.\n"
                          )
                        

                 )
            
     end






(* This helper is needed by main.sml for printing token lists *)
fun tokenToString tok =
    case tok of
        TOK_INT i => "TOK_INT(" ^ (Int.toString i) ^ ")"
      | TOK_IDENTIFIER s => "TOK_IDENTIFIER(\"" ^ s ^ "\")"
      | TOK_OPERATOR s => "TOK_OPERATOR(\"" ^ s ^ "\")"
      | TOK_LPAREN => "TOK_LPAREN"
      | TOK_RPAREN => "TOK_RPAREN"
      | TOK_ARROW => "TOK_ARROW"
      | TOK_BACKSLASH => "TOK_BACKSLASH"
      | TOK_EQUALS => "TOK_EQUALS"
      | TOK_KEYWORD s => "TOK_KEYWORD(\"" ^ s ^ "\")"
      | TOK_STRING s => "TOK_STRING(\"" ^ s ^ "\")"
      | TOK_CHAR c => "TOK_CHAR('" ^ (Char.toString c) ^ "')"
      | TOK_EOF => "TOK_EOF"
      | TOK_ERROR s => "TOK_ERROR(\"" ^ s ^ "\")"
      | _ => "UNKNOWN_TOKEN_TYPE"
      
      


fun tokenListToString [] = "[]"
  | tokenListToString [tok] = "[" ^ (tokenToString tok) ^ "]"
  | tokenListToString (tok :: toks) = "[" ^ (tokenToString tok) ^ ", " ^ (String.concatWith ", " (List.map tokenToString toks)) ^ "]";




