package monkey.lexer

import monkey.token
import monkey.token.Token

case class Lexer(input: String) {
  private var position = 0
  private var readPosition = 0
  private var ch = '\0'

  readChar()

  def nextToken(): Token = {
    skipWhitespace()

    val tok = ch match {
      case '=' =>
        if (peekChar() == '=') {
          val ch = this.ch
          readChar()
          val literal = ch.toString + this.ch.toString
          token.Token(Token.EQ, literal)
        } else {
          Token(Token.ASSIGN, ch)
        }
      case '+' => Token(Token.PLUS, ch)
      case '-' => Token(Token.MINUS, ch)
      case '!' =>
        if (peekChar() == '=') {
          val ch = this.ch
          readChar()
          val literal = ch.toString + this.ch.toString
          token.Token(Token.NOT_EQ, literal)
        } else {
          Token(Token.BANG, ch)
        }
      case '/'  => Token(Token.SLASH, ch)
      case '*'  => Token(Token.ASTERISK, ch)
      case '<'  => Token(Token.LT, ch)
      case '>'  => Token(Token.GT, ch)
      case ';'  => Token(Token.SEMICOLON, ch)
      case ':'  => Token(Token.COLON, ch)
      case '('  => Token(Token.LPAREN, ch)
      case ')'  => Token(Token.RPAREN, ch)
      case ','  => Token(Token.COMMA, ch)
      case '{'  => Token(Token.LBRACE, ch)
      case '}'  => Token(Token.RBRACE, ch)
      case '['  => Token(Token.LBRACKET, ch)
      case ']'  => Token(Token.RBRACKET, ch)
      case '"'  => token.Token(Token.STRING, readString())
      case '\0' => token.Token(Token.EOF, "")
      case _ =>
        if (isLetter(this.ch)) {
          val i = readIdentifier()
          return token.Token(Token.lookupIdent(i), i)
        } else if (isDigit(this.ch)) {
          return token.Token(Token.INT, readNumber())
        } else {
          return Token(Token.ILLEGAL, this.ch)
        }
    }

    readChar()
    tok
  }

  private def readIdentifier(): String = {
    val pos = position
    while (isLetter(ch)) {
      readChar()
    }
    input.slice(pos, position)
  }

  private def readNumber(): String = {
    val pos = position
    while (isDigit(ch)) {
      readChar()
    }
    input.slice(pos, position)
  }

  private def readChar(): Unit = {
    if (readPosition >= input.length) {
      ch = 0.toChar
    } else {
      ch = input(readPosition)
    }
    position = readPosition
    readPosition += 1
  }

  private def readString(): String = {
    val pos = position + 1
    do {
      readChar()
    } while (ch != '"' && ch != 0)
    input.slice(pos, position)
  }

  def isDigit(ch: Char): Boolean = {
    '0' <= ch && ch <= '9'
  }

  def isLetter(ch: Char): Boolean = {
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '_')
  }

  def skipWhitespace(): Unit = {
    while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
      readChar()
    }
  }

  def peekChar(): Char = {
    if (readPosition >= input.length) {
      0.toChar
    } else {
      input(readPosition)
    }
  }
}
