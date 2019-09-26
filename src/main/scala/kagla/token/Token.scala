package kagla.token

import kagla.token.Token.TokenType

object Token {
  type TokenType = String

  val ILLEGAL: TokenType = "ILLEGAL"
  val EOF: TokenType = "EOF"

  // identifiers + literals
  val IDENT: TokenType = "IDENT"
  val INT: TokenType = "INT"
  val STRING: TokenType = "STRING"

  // operators
  val ASSIGN: TokenType = "="
  val PLUS: TokenType = "+"
  val MINUS: TokenType = "-"
  val BANG: TokenType = "!"
  val ASTERISK: TokenType = "*"
  val SLASH: TokenType = "/"
  val LT: TokenType = "<"
  val GT: TokenType = ">"

  val EQ: TokenType = "=="
  val NOT_EQ: TokenType = "!="

  // delimiters
  val COMMA: TokenType = ","
  val SEMICOLON: TokenType = ";"
  val COLON: TokenType = ":"

  val LPAREN: TokenType = "("
  val RPAREN: TokenType = ")"
  val LBRACE: TokenType = "{"
  val RBRACE: TokenType = "}"
  val LBRACKET: TokenType = "["
  val RBRACKET: TokenType = "]"

  // keywords
  val FUNCTION: TokenType = "FUNCTION"
  val LET: TokenType = "LET"
  val TRUE: TokenType = "TRUE"
  val FALSE: TokenType = "FALSE"
  val IF: TokenType = "IF"
  val ELSE: TokenType = "ELSE"
  val RETURN: TokenType = "RETURN"

  val keywords: Map[String, TokenType] =
    Map("fn" -> FUNCTION,
        "let" -> LET,
        "true" -> TRUE,
        "false" -> FALSE,
        "if" -> IF,
        "else" -> ELSE,
        "return" -> RETURN)

  def apply(tokenType: TokenType, ch: Char): Token =
    Token(tokenType, ch.toString)

  def lookupIdent(ident: String): TokenType = keywords.getOrElse(ident, IDENT)
}

case class Token(tokenType: TokenType, literal: String)

