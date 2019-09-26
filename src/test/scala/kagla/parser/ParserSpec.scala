package kagla.parser

import kagla.ast
import kagla.ast.{ArrayLiteral, CallExpression, Expression, ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLIteral, LetStatement, PrefixExpression, ReturnStatement, Statement, StringLIteral}
import kagla.lexer.Lexer
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "A Parser" should "parse the LET statement" in {
    val tests = List(
      ("let x = 5;", "x", 5),
      ("let y = true;", "y", true),
      ("let foobar = y;", "foobar", "y"),
    )

    tests.foreach {
      case (input, expectedIdentifier, expectedValue) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.length shouldBe 1

        val stmt = program.statements.head
        testLetStatement(stmt, expectedIdentifier)

        val letStmt = stmt.asInstanceOf[LetStatement]
        val value   = letStmt.value
        testLiteralExpression(value, expectedValue)
      }
    }
  }

  "A Parser" should "parse the RETURN statement" in {
    val tests = List(
      ("return 5;", 5),
      ("return true;", true),
      ("return foobar;", "foobar"),
    )

    tests.foreach {
      case (input, expectedValue) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.length shouldBe 1

        val stmt       = program.statements.head
        val returnStmt = stmt.asInstanceOf[ReturnStatement]
        returnStmt.tokenLiteral() shouldBe "return"
        testLiteralExpression(returnStmt.returnValue, expectedValue)
      }
    }
  }

  "A Parser" should "parse the Identifier expression" in {
    val input = "foobar;"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    program.statements.length shouldBe 1

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[Identifier]
    val ident = exp.asInstanceOf[Identifier]
    ident.value shouldBe "foobar"
    ident.tokenLiteral() shouldBe "foobar"
  }

  "A Parser" should "parse the Integer literal expression" in {
    val input = "5;"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    program.statements.length shouldBe 1

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[IntegerLIteral]
    val literal = exp.asInstanceOf[IntegerLIteral]
    literal.value shouldBe 5
    literal.tokenLiteral() shouldBe "5"
  }

  "A Parser" should "parse the Prefix expressions" in {
    val tests = List(
      ("!5;", "!", 5),
      ("-15;", "-", 15),
      ("!foobar;", "!", "foobar"),
    )

    tests.foreach {
      case (input, operator, value) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.length shouldBe 1

        val stmt = program.statements.head
        stmt shouldBe a[ExpressionStatement]
        val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

        val exp = expressionStmt.expression.get
        exp shouldBe a[PrefixExpression]
        val prefixExp = exp.asInstanceOf[PrefixExpression]
        prefixExp.operator shouldBe operator
        testLiteralExpression(prefixExp.right, value)
      }
    }
  }

  "A Parser" should "parse the Infix expressions" in {
    val tests = List(
      ("5 + 5;", 5, "+", 5),
      ("5 - 5;", 5, "-", 5),
      ("5 * 5;", 5, "*", 5),
      ("5 / 5;", 5, "/", 5),
      ("5 > 5;", 5, ">", 5),
      ("5 < 5;", 5, "<", 5),
      ("5 == 5;", 5, "==", 5),
      ("5 != 5;", 5, "!=", 5),
      ("foobar + barfoo;", "foobar", "+", "barfoo"),
      ("foobar - barfoo;", "foobar", "-", "barfoo"),
      ("foobar * barfoo;", "foobar", "*", "barfoo"),
      ("foobar / barfoo;", "foobar", "/", "barfoo"),
      ("foobar > barfoo;", "foobar", ">", "barfoo"),
      ("foobar < barfoo;", "foobar", "<", "barfoo"),
      ("foobar == barfoo;", "foobar", "==", "barfoo"),
      ("foobar != barfoo;", "foobar", "!=", "barfoo"),
      ("true == true", true, "==", true),
      ("true != false", true, "!=", false),
      ("false == false", false, "==", false),
    )

    tests.foreach {
      case (input, leftValue, operator, rightValue) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.length shouldBe 1

        val stmt = program.statements.head
        stmt shouldBe a[ExpressionStatement]
        val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

        testInfixExpression(expressionStmt.expression.get, leftValue, operator, rightValue)
      }
    }
  }

  "A Parser" should "parse the operator precedence" in {
    val tests = List(
      (
        "-a * b",
        "((-a) * b)",
      ),
      (
        "!-a",
        "(!(-a))",
      ),
      (
        "a + b + c",
        "((a + b) + c)",
      ),
      (
        "a + b - c",
        "((a + b) - c)",
      ),
      (
        "a * b * c",
        "((a * b) * c)",
      ),
      (
        "a * b / c",
        "((a * b) / c)",
      ),
      (
        "a + b / c",
        "(a + (b / c))",
      ),
      (
        "a + b * c + d / e - f",
        "(((a + (b * c)) + (d / e)) - f)",
      ),
      (
        "3 + 4; -5 * 5",
        "(3 + 4)((-5) * 5)",
      ),
      (
        "5 > 4 == 3 < 4",
        "((5 > 4) == (3 < 4))",
      ),
      (
        "5 < 4 != 3 > 4",
        "((5 < 4) != (3 > 4))",
      ),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
      ),
      (
        "true",
        "true",
      ),
      (
        "false",
        "false",
      ),
      (
        "3 > 5 == false",
        "((3 > 5) == false)",
      ),
      (
        "3 < 5 == true",
        "((3 < 5) == true)",
      ),
      (
        "1 + (2 + 3) + 4",
        "((1 + (2 + 3)) + 4)",
      ),
      (
        "(5 + 5) * 2",
        "((5 + 5) * 2)",
      ),
      (
        "2 / (5 + 5)",
        "(2 / (5 + 5))",
      ),
      (
        "(5 + 5) * 2 * (5 + 5)",
        "(((5 + 5) * 2) * (5 + 5))",
      ),
      (
        "-(5 + 5)",
        "(-(5 + 5))",
      ),
      (
        "!(true == true)",
        "(!(true == true))",
      ),
      (
        "a + add(b * c) + d",
        "((a + add((b * c))) + d)",
      ),
      (
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
      ),
      (
        "add(a + b + c * d / f + g)",
        "add((((a + b) + ((c * d) / f)) + g))",
      ),
      (
        "a * [1, 2, 3, 4][b * c] * d",
        "((a * ([1, 2, 3, 4][(b * c)])) * d)",
      ),
      (
        "add(a * b[2], b[1], 2 * [1, 2][1])",
        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
      ),
    )
    tests.foreach {
      case (input, expected) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        val actual = program.String()
        actual shouldBe expected
      }
    }
  }

  "A Parser" should "parse the Boolean expression" in {
    val tests = List(
      ("true;", true),
      ("false;", false),
    )

    tests.foreach {
      case (input, expectedBoolean) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.length shouldBe 1

        program.statements.head shouldBe a[ExpressionStatement]
        val stmt = program.statements.head.asInstanceOf[ExpressionStatement]

        stmt.expression.get shouldBe a[ast.Boolean]
        val boolean = stmt.expression.get.asInstanceOf[ast.Boolean]
        boolean.value shouldBe expectedBoolean
      }
    }
  }

  "A Parser" should "parse the If expression" in {
    val input = "if (x < y) { x }"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    program.statements.length shouldBe 1

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[IfExpression]
    val ifExp = exp.asInstanceOf[IfExpression]

    testInfixExpression(ifExp.condition.get, "x", "<", "y")

    ifExp.consequence.statements.length shouldBe 1
    ifExp.consequence.statements.head shouldBe a[ExpressionStatement]

    val consequence = ifExp.consequence.statements.head.asInstanceOf[ExpressionStatement]
    testIdentifier(consequence.expression.get, "x")

    ifExp.alternative shouldBe None
  }

  "A Parser" should "parse the IfElse expression" in {
    val input = "if (x < y) { x } else { y }"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    program.statements.length shouldBe 1

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[IfExpression]
    val ifExp = exp.asInstanceOf[IfExpression]

    testInfixExpression(ifExp.condition.get, "x", "<", "y")

    ifExp.consequence.statements.length shouldBe 1
    ifExp.consequence.statements.head shouldBe a[ExpressionStatement]

    val consequence = ifExp.consequence.statements.head.asInstanceOf[ExpressionStatement]
    testIdentifier(consequence.expression.get, "x")

    val alt = ifExp.alternative.get
    alt.statements.length shouldBe 1
    alt.statements.head shouldBe a[ExpressionStatement]

    val alternative = alt.statements.head.asInstanceOf[ExpressionStatement]
    testIdentifier(alternative.expression.get, "y")
  }

  "A Parser" should "parse the function literal" in {
    val input = "fn(x, y) { x + y; }"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    program.statements.length shouldBe 1

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[FunctionLiteral]
    val function = exp.asInstanceOf[FunctionLiteral]

    val params = function.parameters.get
    params.length shouldBe 2
    testLiteralExpression(Some(params.head), "x")
    testLiteralExpression(Some(params(1)), "y")

    val bodyStmts = function.body.statements
    bodyStmts.length shouldBe 1

    bodyStmts.head shouldBe a[ExpressionStatement]
    val bodyStmt = bodyStmts.head.asInstanceOf[ExpressionStatement]
    testInfixExpression(bodyStmt.expression.get, "x", "+", "y")
  }

  "A Parser" should "parse the function parameters" in {
    val tests = List(
      ("fn() {};", List()),
      ("fn(x) {};", List("x")),
      ("fn(x, y, z) {};", List("x", "y", "z")),
    )

    tests.foreach {
      case (input, expectedParams) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.head shouldBe a[ExpressionStatement]
        val stmt = program.statements.head.asInstanceOf[ExpressionStatement]
        stmt.expression.get shouldBe a[FunctionLiteral]
        val function = stmt.expression.get.asInstanceOf[FunctionLiteral]

        val params = function.parameters.get
        params.length shouldBe expectedParams.length

        params zip expectedParams foreach { t =>
          testLiteralExpression(Some(t._1), t._2)
        }
      }
    }
  }

  "A Parser" should "parse the call expression" in {
    val input = "add(1, 2 * 3, 4 + 5);"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    program.statements.length shouldBe 1

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[CallExpression]
    val callExp = exp.asInstanceOf[CallExpression]

    testIdentifier(callExp.function, "add")

    val arguments = callExp.arguments.get
    arguments.length shouldBe 3

    testLiteralExpression(arguments.head, 1)
    testInfixExpression(arguments(1).get, 2, "*", 3)
    testInfixExpression(arguments(2).get, 4, "+", 5)
  }

  "A Parser" should "parse the call expression parameters" in {
    val tests = List(
      ("add();", "add", List()),
      ("add(1);", "add", List("1")),
      ("add(1, 2 * 3, 4 + 5);", "add", List("1", "(2 * 3)", "(4 + 5)")),
    )

    tests.foreach {
      case (input, expectedIdent, expectedArgs) => {
        val l       = Lexer(input)
        val p       = Parser(l)
        val program = p.parseProgram()
        checkParserErrors(p)

        program.statements.head shouldBe a[ExpressionStatement]
        val stmt = program.statements.head.asInstanceOf[ExpressionStatement]
        stmt.expression.get shouldBe a[CallExpression]
        val callExp = stmt.expression.get.asInstanceOf[CallExpression]

        testIdentifier(callExp.function, expectedIdent)

        val arguments = callExp.arguments.get
        arguments.length shouldBe expectedArgs.length

        arguments zip expectedArgs foreach { t =>
          t._1.get.String() shouldBe t._2
        }
      }
    }
  }

  "A Parser" should "parse the string literal" in {
    val input = """"hello world";"""

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[StringLIteral]
    val literal = exp.asInstanceOf[StringLIteral]

    literal.value shouldBe "hello world"
  }

  "A Parser" should "parse the array literals" in {
    val input = "[1, 2 * 2, 3 + 3]"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[ArrayLiteral]
    val array = exp.asInstanceOf[ArrayLiteral]

    val elements = array.elements.get
    elements.length shouldBe 3

    testIntegerLiteral(elements.head.get, 1)
    testInfixExpression(elements(1).get, 2, "*", 2)
    testInfixExpression(elements(2).get, 3, "+", 3)
  }

  "A Parser" should "parse the index expressions" in {
    val input = "myArray[1 + 1]"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[IndexExpression]
    val indexExp = exp.asInstanceOf[IndexExpression]

    testIdentifier(indexExp.left, "myArray")

    testInfixExpression(indexExp.index.get, 1, "+", 1)
  }

  "A Parser" should "parse the hash literals string keys" in {
    val input = """{"one": 1, "two": 2, "three": 3}"""

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[HashLiteral]
    val hash = exp.asInstanceOf[HashLiteral]

    hash.pairs.size shouldBe 3

    val expected = Map(
      "one" ->   1,
      "two" ->   2,
      "three" -> 3,
    )

    hash.pairs.foreach {case (maybeKey, maybeValue) =>
      val key = maybeKey.get
      val value = maybeValue.get
      key shouldBe a[StringLIteral]
      val literal = key.asInstanceOf[StringLIteral]

      val expectedValue = expected(literal.String())

      testIntegerLiteral(value, expectedValue)
    }
  }

  "A Parser" should "parse the empty hash literal" in {
    val input = "{}"

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[HashLiteral]
    val hash = exp.asInstanceOf[HashLiteral]

    hash.pairs.size shouldBe 0
  }

  "A Parser" should "parse the hash literals with expressions" in {
    val input = """{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"""

    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    checkParserErrors(p)

    val stmt = program.statements.head
    stmt shouldBe a[ExpressionStatement]
    val expressionStmt = stmt.asInstanceOf[ExpressionStatement]

    val exp = expressionStmt.expression.get
    exp shouldBe a[HashLiteral]
    val hash = exp.asInstanceOf[HashLiteral]

    hash.pairs.size shouldBe 3

    val tests = Map(
      "one" -> ((e: Expression) => testInfixExpression(e, 0, "+", 1)),
      "two" -> ((e: Expression) => testInfixExpression(e, 10, "-", 8)),
      "three" -> ((e: Expression) => testInfixExpression(e, 15, "/", 5))
    )

    hash.pairs.foreach {case (maybeKey, maybeValue) =>
      val key = maybeKey.get
      val value = maybeValue.get
      key shouldBe a[StringLIteral]
      val literal = key.asInstanceOf[StringLIteral]

      val testFunc = tests.get(literal.String())
      testFunc should not be None
      testFunc.get(value)
    }
  }

  def checkParserErrors(p: Parser): Unit = {
    val errors = p.errors

    if (errors.isEmpty) {
      return
    }

    info(s"parser has ${errors.length} errors")
    errors.foreach(err => info(s"parser error: $err"))
    errors.length shouldBe 0
  }

  def testLetStatement(s: Statement, name: String): Unit = {
    s.tokenLiteral() shouldBe "let"
    s shouldBe a[LetStatement]

    val letStmt = s.asInstanceOf[LetStatement]
    letStmt.name.value shouldBe name
    letStmt.name.tokenLiteral() shouldBe name
  }

  def testLiteralExpression(maybeExp: Option[Expression], expected: Any): Unit = maybeExp match {
    case Some(exp) =>
      expected match {
        case _: Int     => testIntegerLiteral(exp, expected.asInstanceOf[Int])
        case _: Long    => testIntegerLiteral(exp, expected.asInstanceOf[Long])
        case _: String  => testIdentifier(exp, expected.asInstanceOf[String])
        case _: Boolean => testBooleanLiteral(exp, expected.asInstanceOf[Boolean])
      }
    case None => fail("Expression is empty")
  }

  def testInfixExpression(exp: Expression, left: Any, operator: String, right: Any): Unit = {
    exp shouldBe a[InfixExpression]
    val opExp = exp.asInstanceOf[InfixExpression]

    testLiteralExpression(Some(opExp.left), left)

    opExp.operator shouldBe operator

    testLiteralExpression(opExp.right, right)
  }

  def testIntegerLiteral(il: Expression, value: Long): Unit = {
    il shouldBe a[IntegerLIteral]

    val integ = il.asInstanceOf[IntegerLIteral]
    integ.value shouldBe value

    integ.tokenLiteral() shouldBe value.toString
  }

  def testIdentifier(exp: Expression, value: String): Unit = {
    exp shouldBe a[Identifier]

    val ident = exp.asInstanceOf[Identifier]
    ident.value shouldBe value

    ident.tokenLiteral() shouldBe value
  }

  def testBooleanLiteral(exp: Expression, value: Boolean): Unit = {
    exp shouldBe a[ast.Boolean]

    val bo = exp.asInstanceOf[ast.Boolean]
    bo.value shouldBe value

    bo.tokenLiteral() shouldBe value.toString
  }
}
