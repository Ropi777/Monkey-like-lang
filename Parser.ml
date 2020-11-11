(* Takes list of token as input checks for syntactic errors, and produces ast*)

type expression = Expr of  equality

and equality = EQ of comparison * eq_operator  * comparison

and eq_operator = EQUAL_EQUAL | BANG_EQUAL

and comparison = COMP of  term * comp_operator * term

and comp_operator = GREATER| GREATER_EQUAL| LESS| LESS_EQUAL

and term = TRM of factor * term_operator  * factor

and term_operator = MINUS| PLUS

and factor = FACT of unary  * factor_operator * unary

and factor_operator = SLASH | STAR

and unary = |UNAR of unary_operator * unary
            |PRIM of primary

and unary_operator = BANG| MINUS

and primary = |NUMBER of float |STRING of string | TRUE | FALSE |NIL |GRP of lparen * expression * rparen

and lparen = LPAREN

and rparen = RPAREN



let parse token_list =
  let parse_list token_list ast  = 
      match token_list with
      (h::t) ->
        match h with
        | EOF -> 
        | ERROR -> 

