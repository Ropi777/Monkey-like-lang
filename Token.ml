type token =
  |EOF
  |ERROR of string

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
  |QUESTION_MARK

  (*Delimiters*)
  |SEMICOLON
  |COMMA
  |DOT

  (*Keywords*)
  |VAR
  |FUNCTION
  |CLASS
  |IF
  |ELSE
  |AND
  |OR
  |RETURN
  |NIL
  |WHILE
  |FOR
  |TRUE
  |FALSE
  |PRINT
  |SUPER
  |THIS

  (*Identifiers and literals*)
  |NUMBER of float
  |IDENTIFIER of string
  |STRING of string

