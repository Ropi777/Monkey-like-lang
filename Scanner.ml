#use "Token.ml"

let lexical_analysis source =

  let len = String.length source in

  let matches pos expected =
    if pos = len then false
    else if (String.get source pos) != expected then false
    else true in

  let rec skip_line pos =
    if (String.get source pos) = '\n' then (pos + 1)
    else skip_line (pos + 1)
  in

  let rec check_string pos =
    if pos = len then (false, -1)
    else if (String.get source pos) = '"' then (true, pos)
    else check_string (pos + 1) in

  let rec make_string strt ed =
    String.sub source strt (ed - strt + 1) in

  let make_number start =
    let rec get_position ennd dot =
      if ennd = len then ennd else 
      match String.get source ennd   with
      | '0'..'9' -> get_position (ennd + 1) dot
      | '.' -> if dot then ennd
               else get_position (ennd + 1) true
      | _ -> ennd
     
      in let ende = get_position start false
      in (Stdlib.float_of_string(String.sub source start (ende - start))
         , ende) in


  let is_alpha_numeric chr =
    match chr with
    | 'a'..'z'| 'A'..'Z' | '0'..'9' | '_' -> true
    | _ -> false in


  let create_identifier start =
    let rec get_position ennd =
      if ennd = len then ennd else
      if is_alpha_numeric (String.get source ennd)
      then get_position (ennd + 1)
      else ennd
    in let ende = get_position start
    in (String.sub source start (ende - start), ende) in


  let rec analyze current_pos token_list=
    if current_pos = len then (EOF::token_list)
    else
    match String.get source current_pos with
    | '{' -> analyze (current_pos + 1) (LBRACE::token_list)
    | '}' -> analyze (current_pos + 1) (RBRACE::token_list) 
    | '(' -> analyze (current_pos + 1) (LPAREN::token_list)
    | ')' -> analyze (current_pos + 1) (RPAREN::token_list)
    | ';' -> analyze (current_pos + 1) (SEMICOLON::token_list)
    | ',' -> analyze (current_pos + 1) (COMMA::token_list)
    | '+' -> analyze (current_pos + 1) (PLUS::token_list)
    | '-' -> analyze (current_pos + 1) (MINUS::token_list)
    | '*' -> analyze (current_pos + 1) (STAR::token_list)
    | '?' -> analyze (current_pos + 1) (QUESTION_MARK::token_list)
    | '.' -> analyze (current_pos + 1) (DOT::token_list)
    | '!' -> if (matches (current_pos + 1) '=') then
                  analyze (current_pos + 2) (BANG_EQUAL::token_list)
             else analyze (current_pos + 1) (BANG::token_list)
    | '<' -> if (matches (current_pos + 1) '=') then
                  analyze (current_pos + 2) (GREATER_EQUAL::token_list)
             else analyze (current_pos + 1) (GREATER::token_list)
    | '>' -> if (matches (current_pos + 1) '=') then
                  analyze (current_pos + 2) (LESS_EQUAL::token_list)
             else analyze (current_pos + 1)(LESS::token_list)
    | '=' -> if (matches (current_pos + 1) '=') then
           analyze (current_pos + 2) (EQUAL_EQUAL::token_list)
          else analyze (current_pos + 1)(EQUAL::token_list)
    | '/' -> if (matches (current_pos + 1) '/') then
                  analyze (skip_line (current_pos + 1)) token_list
             else analyze (current_pos + 1) (SLASH::token_list)
    | ' '| '\t' | '\r' -> analyze (current_pos + 1) token_list
    | '"' -> let (bl, next_pos) = (check_string (current_pos + 1))
      in
      if (bl, next_pos) = (false, -1)
        then failwith "Error unterminated string"
        else analyze (next_pos + 1)
            (STRING(make_string (current_pos + 1) (next_pos -1))
             ::token_list)
    | '0'..'9' -> let (num, nxt_pos) = make_number current_pos in
                    analyze nxt_pos ((NUMBER(num))::token_list)
    | 'a'..'z' | 'A'..'Z' | '_' ->
      let (ident, nxt_pos) = create_identifier current_pos in
      match ident with
      | "var" -> let tok = VAR in analyze nxt_pos (tok::token_list)
      | "fun" -> let tok = FUNCTION in analyze nxt_pos (tok::token_list)
      | "class" -> let tok = CLASS in analyze nxt_pos (tok::token_list)
      | "if" -> let tok = IF in analyze nxt_pos (tok::token_list)
      | "else" -> let tok = ELSE in analyze nxt_pos (tok::token_list)
      | "and" -> let tok = AND in analyze nxt_pos (tok::token_list)
      | "or" -> let tok = OR in analyze nxt_pos (tok::token_list)
      | "return" -> let tok = RETURN in analyze nxt_pos (tok::token_list)
      | "nil" -> let tok = NIL in analyze nxt_pos (tok::token_list)
      | "while" -> let tok = WHILE in analyze nxt_pos (tok::token_list)
      | "for" -> let tok = FOR in analyze nxt_pos (tok::token_list) 
      | "true" -> let tok = TRUE in analyze nxt_pos (tok::token_list)
      | "false" -> let tok = FALSE in analyze nxt_pos (tok::token_list)
      | "print" -> let tok = PRINT in analyze nxt_pos (tok::token_list)
      | "super" -> let tok = SUPER in analyze nxt_pos (tok::token_list)
      | "this" -> let tok = THIS in analyze nxt_pos (tok::token_list)
      | _ -> let tok = IDENTIFIER(ident) in analyze nxt_pos (tok::token_list)

    | _ -> failwith ("Lexical error, unknown token at line" ^  (string_of_int current_pos) ) 


  in List.rev (analyze 0 [])

