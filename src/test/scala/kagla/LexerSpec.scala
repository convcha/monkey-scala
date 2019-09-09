package kagla

import org.scalatest._
import Token._

class LexerSpec extends FlatSpec with Matchers {
  "nextToken" should "return exact result" in {
    val input =
      """let five = 5;
      let ten = 10;

      let add = fn(x, y) {
      	x + y;
      };

      let result = add(five, ten);
      !-/*5;
      5 < 10 > 5;
      if (5 < 10) {
         return true;
       } else {
         return false;
      }

      10 == 10;
      10 != 9;
      "foobar"
      "foo bar"
      [1, 2];
      {"foo": "bar"}
      """

    val tests = List(
      (LET, "let"),
      (IDENT, "five"),
      (ASSIGN, "="),
      (INT, "5"),
      (SEMICOLON, ";"),
      (LET, "let"),
      (IDENT, "ten"),
      (ASSIGN, "="),
      (INT, "10"),
      (SEMICOLON, ";"),
      (LET, "let"),
      (IDENT, "add"),
      (ASSIGN, "="),
      (FUNCTION, "fn"),
      (LPAREN, "("),
      (IDENT, "x"),
      (COMMA, ","),
      (IDENT, "y"),
      (RPAREN, ")"),
      (LBRACE, "{"),
      (IDENT, "x"),
      (PLUS, "+"),
      (IDENT, "y"),
      (SEMICOLON, ";"),
      (RBRACE, "}"),
      (SEMICOLON, ";"),
      (LET, "let"),
      (IDENT, "result"),
      (ASSIGN, "="),
      (IDENT, "add"),
      (LPAREN, "("),
      (IDENT, "five"),
      (COMMA, ","),
      (IDENT, "ten"),
      (RPAREN, ")"),
      (SEMICOLON, ";"),
      (BANG, "!"),
      (MINUS, "-"),
      (SLASH, "/"),
      (ASTERISK, "*"),
      (INT, "5"),
      (SEMICOLON, ";"),
      (INT, "5"),
      (LT, "<"),
      (INT, "10"),
      (GT, ">"),
      (INT, "5"),
      (SEMICOLON, ";"),
      (IF, "if"),
      (LPAREN, "("),
      (INT, "5"),
      (LT, "<"),
      (INT, "10"),
      (RPAREN, ")"),
      (LBRACE, "{"),
      (RETURN, "return"),
      (TRUE, "true"),
      (SEMICOLON, ";"),
      (RBRACE, "}"),
      (ELSE, "else"),
      (LBRACE, "{"),
      (RETURN, "return"),
      (FALSE, "false"),
      (SEMICOLON, ";"),
      (RBRACE, "}"),
      (INT, "10"),
      (EQ, "=="),
      (INT, "10"),
      (SEMICOLON, ";"),
      (INT, "10"),
      (NOT_EQ, "!="),
      (INT, "9"),
      (SEMICOLON, ";"),
      (STRING, "foobar"),
      (STRING, "foo bar"),
      (LBRACKET, "["),
      (INT, "1"),
      (COMMA, ","),
      (INT, "2"),
      (RBRACKET, "]"),
      (SEMICOLON, ";"),
      (LBRACE, "{"),
      (STRING, "foo"),
      (COLON, ":"),
      (STRING, "bar"),
      (RBRACE, "}"),
      (EOF, "")
    )

    val l = Lexer(input)

    tests.foreach {
      case (expectedType, expectedLiteral) =>
        val tok = l.nextToken()
        tok.tokenType shouldEqual expectedType
        tok.literal shouldEqual expectedLiteral
    }
  }
}
