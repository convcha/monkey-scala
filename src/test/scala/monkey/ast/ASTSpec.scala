package monkey.ast

import monkey.token.Token
import monkey.token.Token._
import org.scalatest._

class ASTSpec extends FlatSpec with Matchers {
  "Program" should "return exact AST tree" in {
    val program = Program(
      statements = List(
        LetStatement(
          token = Token(tokenType = LET, literal = "let"),
          name = Identifier(token = Token(tokenType = IDENT, literal = "myVar"), value = "myVar"),
          value = Some(Identifier(token = Token(tokenType = IDENT, literal = "anotherVar"), value = "anotherVar"))
        )
      )
    )

    program.String() shouldEqual "let myVar = anotherVar;"
  }
}
