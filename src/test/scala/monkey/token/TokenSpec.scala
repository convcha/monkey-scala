package monkey.token

import org.scalatest._

class TokenSpec extends FlatSpec with Matchers {
  "lookupIdent" should "return exact TokenType" in {
    Token.lookupIdent("fn") shouldEqual Token.FUNCTION
    Token.lookupIdent("let") shouldEqual Token.LET
    Token.lookupIdent("true") shouldEqual Token.TRUE
    Token.lookupIdent("false") shouldEqual Token.FALSE
    Token.lookupIdent("if") shouldEqual Token.IF
    Token.lookupIdent("else") shouldEqual Token.ELSE
    Token.lookupIdent("return") shouldEqual Token.RETURN
    Token.lookupIdent("foo") shouldEqual Token.IDENT
    Token.lookupIdent("") shouldEqual Token.IDENT
  }
}
