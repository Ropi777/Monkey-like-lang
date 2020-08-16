type token =
  |ILLEGAL
  |EOF

  (*Braces and Parenthesis*)
  |LPAREN
  |RPAREN
  |LBRACE
  |RBRACE

  (*Operators*)
  |PLUS
  |MINUS
  |STAR
  |SLASH
  |EQUAL
  |EQUAL_EQUAL
  |GREATER
  |GREATER_EQUAL
  |LESS
  |LESS_EQUAL
  |BANG
  |BANG_EQUAL

  (*Delimiters*)
  |SEMICOLON
  |COMMA

  (*Keywords*)
  |LET
  |FUNCTION
  |IF
  |ELSE
  |AND
  |OR
  |RETURN
  |NIL
  |WHILE
  |TRUE
  |FALSE

  (*Identifiers and literals*)
  |INTEGER of int
  |IDENTIFIER of string
  |STRING of string

