datatype token =
    TOK_IDENTIFIER of string   (* e.g., x, myFunc *)
  | TOK_CONSTRUCTOR of string  (* e.g., Just, Nothing, True, False *)
  | TOK_INT of int             (* e.g., 42, -10 *)
  | TOK_OPERATOR of string     (* e.g., +, -, *, /, ==, > *)
  | TOK_STRING of string       (* e.g., "hello", "world\n" *)
  | TOK_CHAR of char           (* e.g., 'a', '\n' *)
  | TOK_LPAREN                 (* ( *)
  | TOK_RPAREN                 (* ) *)
  | TOK_LBRACKET               (* [ *)
  | TOK_RBRACKET               (* ] *)
  | TOK_LBRACE                 (* { *)
  | TOK_RBRACE                 (* } *)
  | TOK_COMMA                  (* , *)
  | TOK_SEMICOLON              (* ; *)
  | TOK_COLON                  (* : *)
  | TOK_DCOLON                 (* :: *)
  | TOK_ARROW                  (* -> *)
  | TOK_EQUALS                 (* = *)
  | TOK_BACKSLASH              (* \ *)
  | TOK_PIPE                   (* | *)
  | TOK_DOT                    (* . *)
  | TOK_EOF                    (* End of input *)
  | TOK_ERROR of string        (* Unrecognized character or sequence *)
  | TOK_KEYWORD of string      (* e.g., if, then, else, let, in *)
  ;




val keywords =
  ["if", "then", "else", "let", "in",  "end" , "do", "module", "import", "where",
   "data", "type", "class", "instance", "deriving", "case", "of", "_","True","False"];

fun isKeyword s = List.exists (fn k => k = s) keywords;



fun isDigit c = Char.isDigit c;

fun isAlpha c = Char.isAlpha c;

fun isAlphaNum c = isAlpha c orelse isDigit c;

fun isWhitespace c = Char.isSpace c; (* Includes space, tab, newline, etc. *)

fun lookup_keyword s =
    if List.exists (fn kw => kw = s) keywords then TOK_KEYWORD s
    else TOK_IDENTIFIER s


fun isOperatorChar c =
    let
        val s = Char.toString c
    in
        s = "+" orelse s = "-" orelse s = "*" orelse s = "/" orelse
        s = "=" orelse s = ">" orelse s = "<" orelse s = "&" orelse
        s = "|" orelse s = "." orelse s = ":" orelse s = "!" orelse
        s = "$" orelse s = "%" orelse s = "^" orelse s = "#" orelse
        s = "~" orelse s = "@" orelse s = "`"  
        
    end;


(* Get the character at a specific index in a string *)
fun charAt (s,i) = String.sub (s,i);

(* Get the substring from start index (inclusive) to end index (exclusive) *)
fun substring (s,start,len) = String.substring (s,start,len);

fun stringSize s = String.size s;


(* Skip whitespace and single-line comments *)
fun skipWhitespaceAndComments s i =
    let
        val len = stringSize s
        (* Recursive helper function to advance the index *)
        fun skip' current_i =
            if current_i >= len then
                current_i (* Reached end of string *)
            else
                let
                    val c = charAt(s,current_i)
                in
                    if isWhitespace c then
                        skip' (current_i + 1) (* Skip whitespace *)
                    else if current_i + 1 < len andalso c = #"-" andalso charAt (s,(current_i + 1)) = #"-"
                    then (* Found '--', è un commento in Haskell *)
                         let
                             fun skipUntilNewline comment_i =
                                 if comment_i < len andalso charAt (s,comment_i) <> #"\n"
                                 then skipUntilNewline (comment_i + 1)
                                 else comment_i
                         in
                             skip' (skipUntilNewline (current_i + 2)) (* Skip comment, then continue skipping from there *)
                         end
                    else
                        current_i (* Not whitespace or start of comment, stop skipping *)
                end
    in
        skip' i (* Start the skipping process *)
    end



fun lexIdentifierOrKeyword s i =
    let
        val len = stringSize s
        fun collect_chars current_i =
            if current_i < len andalso isAlphaNum (charAt (s,current_i)) orelse charAt (s,current_i) = #"'" (* è permesso che gli identificatori abbiamo '*)
            then collect_chars (current_i + 1)
            else current_i
        val end_i = collect_chars i
        val id_str = substring (s,i,(end_i - i))
    in
        if stringSize id_str = 0 then NONE (* Not an identifier *)
        else if isKeyword id_str then SOME (TOK_KEYWORD id_str, end_i)
        else if Char.isUpper (String.sub (id_str,0)) then SOME (TOK_CONSTRUCTOR id_str, end_i)
        else SOME (TOK_IDENTIFIER id_str, end_i)
    end


fun lexNumber s i =
    let
        val len = stringSize s
        val start_i = i

        (* Check for optional leading minus sign *)
        val after_sign_i =
            if i < len andalso charAt( s , i) = #"-"
            then i + 1
            else i

        (* Collect digits after the potential sign *)
        fun collect_digits current_d_i =
            if current_d_i < len andalso isDigit (charAt (s,  current_d_i))
            then collect_digits (current_d_i + 1)
            else current_d_i

        val end_i = collect_digits after_sign_i
        val num_str = substring (s , start_i , (end_i - start_i))

    in
        if stringSize num_str = 0 orelse end_i = after_sign_i then NONE (* No digits found, not a number *)
        else
            case Int.fromString num_str of
                SOME i_val => SOME (TOK_INT i_val, end_i)
              | NONE => NONE (* Should not happen if collect_digits only gets digits, but good for safety *)
    end


fun lexOperator s i =
    let
        val len = stringSize s
        fun collect_op_chars current_i =
            if current_i < len andalso isOperatorChar (charAt (s , current_i))
            then collect_op_chars (current_i + 1)
            else current_i
        val end_i = collect_op_chars i
        val op_str = substring (s,  i , (end_i - i))
    in
        if stringSize op_str = 0 then NONE (* Not an operator *)
        else if op_str = "::" then SOME (TOK_DCOLON, end_i) (* Handle multi-char operators first *)
        else if op_str = "->" then SOME (TOK_ARROW, end_i)
        (* Add more multi-char operators here *)
        else SOME (TOK_OPERATOR op_str, end_i) (* Default to generic operator token *)
    end


fun lexString s i =
    let
        val len = stringSize s
        in
        if i < len andalso charAt (s , i) = #"\""
        then (* Found opening quote *)
             let
                 fun collect_string_chars current_i =
                     if current_i < len
                     then
                         let val c = charAt (s , current_i) 
                         in
                             if c = #"\"" then SOME (current_i + 1) (* Found closing quote *)
                             else if c = #"\\" andalso current_i + 1 < len 
                             then collect_string_chars (current_i + 2) (* Skip escape sequence *)

                             else if c = #"\n" then NONE (* Newline inside string is error *)

                             else collect_string_chars (current_i + 1)
                         end
                     else NONE (* Reached EOF before closing quote *)
                 val closing_quote_i_opt = collect_string_chars (i + 1)
             in
                 case closing_quote_i_opt of
                     SOME end_i => SOME (TOK_STRING (substring (s,  (i + 1),  (end_i - i - 2))), end_i) 

                    |NONE => SOME (TOK_ERROR "Unterminated string literal" , len) 
             end
        else NONE (* Not a string literal *)
    end




fun lexChar s i =
    let
        val len = stringSize s
        in
        if i < len andalso charAt (s,  i) = #"'"
        then 
             if i + 2 < len andalso charAt (s , (i + 2)) = #"'" (* Simple case: 'a' *)
             then SOME (TOK_CHAR (charAt (s , (i + 1))), i + 3)
             else if i + 3 < len andalso charAt (s , i) = #"'" andalso charAt (s , (i + 1)) = #"\\" 
                                 andalso charAt (s , (i + 3)) = #"'" (* Simple escape: '\n' *)
             then 
                  let 
                  val escaped_char = case charAt (s, (i+2)) of
                                           #"n" => #"\n"
                                         | #"t" => #"\t"
                                         | #"\\" => #"\\"
                                         | #"\"" => #"\"" 
                                         | #"'" => #"'"
                                         | _ => charAt (s , (i+2)) (* Basic identity for others *)
                  in
                      SOME (TOK_CHAR escaped_char, i + 4)
                  end
             else NONE (* Not a valid character literal format *)
        else NONE (* Not a character literal *)
    end





fun lexPunctuation s i =
    let
        val len = stringSize s
    in 
        if i < len
        then
            case charAt (s,i) of
                #"(" => SOME (TOK_LPAREN, i + 1)
              | #")" => SOME (TOK_RPAREN, i + 1)
              | #"[" => SOME (TOK_LBRACKET, i + 1)
              | #"]" => SOME (TOK_RBRACKET, i + 1)
              | #"{" => SOME (TOK_LBRACE, i + 1)
              | #"}" => SOME (TOK_RBRACE, i + 1)
              | #"," => SOME (TOK_COMMA, i + 1)
              | #";" => SOME (TOK_SEMICOLON, i + 1)
              | #":" => SOME (TOK_COLON, i + 1) (* Note: "::" handled in lexOperator *)
              | #"=" => SOME (TOK_EQUALS, i + 1) (* Note: "==" handled in lexOperator *)
              | #"\\" => SOME (TOK_BACKSLASH, i + 1)
              | #"|" => SOME (TOK_PIPE, i + 1)
              | #"." => SOME (TOK_DOT, i + 1) (* Note: "." in numbers handled in lexNumber *)
              | _ => NONE (* Not a recognized punctuation character *)
        else NONE
    end



(* ------------------------------------------------------------------------ *)
(* 5. The main lexing function *)
(* ------------------------------------------------------------------------ *)

fun tokenize (input: string) : token list =
    let
        val len = String.size input;

        (*
         * `tokenize_helper` function:
         * Recursively processes the input string from a given index.
         * Returns a list of tokens.
         *)
        fun tokenize_helper (idx: int) : token list =
            if idx >= len then
                [TOK_EOF] (* Reached end of string *)
            else
                let
                    val c = String.sub (input ,idx);
                in
                    (* Skip whitespace *)
                    if isWhitespace c then
                        tokenize_helper (idx + 1)
                    (* Parse numbers *)
                    else if isDigit c then
                        let
                            fun collect_digits k acc =
                                if k < len andalso isDigit (String.sub (input ,k)) then
                                    collect_digits (k + 1) (acc ^ (str (String.sub (input,  k))))
                                else
                                    (acc, k)
                            val (num_str, next_idx) = collect_digits idx ""
                            val num = Option.valOf (Int.fromString num_str) (* Convert string to int *)
                        in
                            TOK_INT num :: tokenize_helper next_idx
                        end
                    (* Parse identifiers and keywords *)
                    else if isAlpha c then
                        let
                            fun collect_alphanum k acc =
                                if k < len andalso isAlphaNum (String.sub( input, k)) then
                                    collect_alphanum (k + 1) (acc ^ (str (String.sub (input, k))))
                                else
                                    (acc, k)
                            val (id_str, next_idx) = collect_alphanum idx ""
                        in
                            (lookup_keyword id_str) :: tokenize_helper next_idx
                        end
                    (* Parse string literals *)
                    else if c = #"\"" then
                        (case lexString input idx  of (* Call lexString, starting after the opening quote *)
                             SOME (token, next_idx) => token :: tokenize_helper next_idx
                           | NONE => [TOK_ERROR "Malformed string literal", TOK_EOF]) (* Should not happen with current lexString logic, but good for safety *)
                    (* Parse character literals *)
                    else if c = #"'" then
                        (case lexChar input idx of (* Call lexChar, starting after the opening quote *)
                             SOME (token, next_idx) => token :: tokenize_helper next_idx
                           | NONE => [TOK_ERROR "Malformed character literal", TOK_EOF])
                    (* Parse operators and symbols *)
                    else if isOperatorChar c then
                        let
                            fun collect_op_chars k acc =
                                if k < len andalso isOperatorChar (String.sub (input , k)) then
                                    collect_op_chars (k + 1) (acc ^ (str (String.sub (input, k))))
                                else
                                    (acc, k)
                            val (op_str, next_idx) = collect_op_chars idx ""
                        in
                            (case op_str of
                                 "(" => TOK_LPAREN
                               | ")" => TOK_RPAREN
                               | "->" => TOK_ARROW
                               | "++" => TOK_OPERATOR "++"
                               | "\\" => TOK_BACKSLASH (* Handle single backslash *)
                               | "=" => TOK_EQUALS
                               | other_op => TOK_OPERATOR other_op) :: tokenize_helper next_idx
                        end
                    (* Handle single characters that aren't part of multi-char tokens *)
                    else (
                        case c of 
                            #"(" => TOK_LPAREN :: tokenize_helper (idx + 1)
                          | #")" => TOK_RPAREN :: tokenize_helper (idx + 1)
                          | #"\\" => TOK_BACKSLASH :: tokenize_helper (idx + 1)
                          | #";" => TOK_SEMICOLON :: tokenize_helper (idx + 1)
                          | #"=" => TOK_EQUALS :: tokenize_helper (idx + 1)
                          | #"-" => (* Potential start of -> or a negative number (handled by parser's precedence) or a unary minus (not implemented yet) *)
                              if idx + 1 < len andalso (String.sub (input ,(idx + 1))) = #">" then
                                  TOK_ARROW :: tokenize_helper (idx + 2)
                              else
                                  TOK_OPERATOR "-" :: tokenize_helper (idx + 1) (* Treat as unary/binary minus for now *)
                          | _ => raise Fail ("Lexical error: Unexpected character '" ^ (str c) ^ "' at position " ^ (Int.toString idx))
                    )
                end
    in
        tokenize_helper 0
    end;




(* Call the tokenize function with the example code *)

(* Print the resulting list of tokens *)
(* We need a helper function to print the tokens nicely *)

fun tokenToString tok =
    case tok of
        TOK_IDENTIFIER s => "TOK_IDENTIFIER(\"" ^ s ^ "\")"
      | TOK_CONSTRUCTOR s => "TOK_CONSTRUCTOR(\"" ^ s ^ "\")"
      | TOK_INT i => "TOK_INT(" ^ (Int.toString i) ^ ")"
      | TOK_OPERATOR s => "TOK_OPERATOR(\"" ^ s ^ "\")"
      | TOK_STRING s => "TOK_STRING(\"" ^ s ^ "\")" (* Simplified, doesn't handle escaped quotes *)
      | TOK_CHAR c => "TOK_CHAR('" ^ (Char.toString c) ^ "')" (* Simplified *)
      | TOK_LPAREN => "TOK_LPAREN"
      | TOK_RPAREN => "TOK_RPAREN"
      | TOK_LBRACKET => "TOK_LBRACKET"
      | TOK_RBRACKET => "TOK_RBRACKET"
      | TOK_LBRACE => "TOK_LBRACE"
      | TOK_RBRACE => "TOK_RBRACE"
      | TOK_COMMA => "TOK_COMMA"
      | TOK_SEMICOLON => "TOK_SEMICOLON"
      | TOK_COLON => "TOK_COLON"
      | TOK_DCOLON => "TOK_DCOLON"
      | TOK_ARROW => "TOK_ARROW"
      | TOK_EQUALS => "TOK_EQUALS"
      | TOK_BACKSLASH => "TOK_BACKSLASH"
      | TOK_PIPE => "TOK_PIPE"
      | TOK_DOT => "TOK_DOT"
      | TOK_EOF => "TOK_EOF"
      | TOK_ERROR s => "TOK_ERROR(\"" ^ s ^ "\")"
      | TOK_KEYWORD s => "TOK_KEYWORD(\"" ^ s ^ "\")";

fun printTokens [] = print "]\n"
  | printTokens [tok] = (print (tokenToString tok); print "]\n")
  | printTokens (tok :: toks) = (print (tokenToString tok); print ", "; printTokens toks);

fun printTokenList tokens =
    (print "[";
     printTokens tokens);

fun tokenListToString tokens =
    String.concat ["[", (String.concatWith ", " (List.map tokenToString tokens)), "]"];
    
(* Execute the printing *)
