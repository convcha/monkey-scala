package kagla.evaluator

import kagla.`object`.Environment._
import kagla.`object`.{KArray, KBoolean, KError, KFunction, KHash, KInteger, KString}
import kagla.evaluator.Evaluator._
import kagla.`object`.KObject.KObject
import kagla.lexer.Lexer
import kagla.parser.Parser
import org.scalatest._

class EvaluatorSpec extends FlatSpec with Matchers {
  "A Evaluator" should "evaluate Integer expression" in {
    val tests = List(
      ("5", 5),
      ("10", 10),
      ("-5", -5),
      ("-10", -10),
      ("5 + 5 + 5 + 5 - 10", 10),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-50 + 100 + -50", 0),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)
        testIntegerObject(evaluated, expected)
      }
    }
  }

  "A Evaluator" should "evaluate Boolean expression" in {
    val tests = List(
      ("true", true),
      ("false", false),
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true),
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)
        testBooleanObject(evaluated, expected)
      }
    }
  }

  "A Evaluator" should "evaluate Bang operator" in {
    val tests = List(
      ("!true", false),
      ("!false", true),
      ("!5", false),
      ("!!true", true),
      ("!!false", false),
      ("!!5", true),
    )
    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)
        testBooleanObject(evaluated, expected)
      }
    }
  }

  "A Evaluator" should "evaluate IfElse expressions" in {
    val tests = List(
      ("if (true) { 10 }", 10),
      ("if (false) { 10 }", null),
      ("if (1) { 10 }", 10),
      ("if (1 < 2) { 10 }", 10),
      ("if (1 > 2) { 10 }", null),
      ("if (1 > 2) { 10 } else { 20 }", 20),
      ("if (1 < 2) { 10 } else { 20 }", 10),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)

        expected match {
          case integer: Int => testIntegerObject(evaluated, integer)
          case _            => testNullObject(evaluated)
        }
      }
    }
  }

  "A Evaluator" should "evaluate Return statements" in {
    val tests = List(
      ("return 10;", 10),
      ("return 10; 9;", 10),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      ("""
          |
          |if (10 > 1) {
          |	 if (10 > 1) {
          |		 return 10;
          |	 }
          |
          |	 return 1;
          |}
          |""".stripMargin,
       10)
    )
    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)
        testIntegerObject(evaluated, expected)
      }
    }
  }

  "A Evaluator" should "handle the error" in {
    val tests = List(
      (
        "5 + true;",
        "type mismatch: INTEGER + BOOLEAN",
      ),
      (
        "5 + true; 5;",
        "type mismatch: INTEGER + BOOLEAN",
      ),
      (
        "-true",
        "unknown operator: -BOOLEAN",
      ),
      (
        "true + false;",
        "unknown operator: BOOLEAN + BOOLEAN",
      ),
      (
        "5; true + false; 5",
        "unknown operator: BOOLEAN + BOOLEAN",
      ),
      (
        "if (10 > 1) { true + false; }",
        "unknown operator: BOOLEAN + BOOLEAN",
      ),
      (
        """
          |
          |if (10 > 1) {
          |  if (10 > 1) {
          |    return true + false;
          |  }
          |
          |  return 1;
          |}
          |""".stripMargin,
        "unknown operator: BOOLEAN + BOOLEAN",
      ),
      (
        "foobar",
        "identifier not found: foobar",
      ),
      (
        """"Hello" - "World"""",
        "unknown operator: STRING - STRING",
      ),
      (
        """{"name": "Monkey"}[fn(x) { x }];""",
        "unusable as hash key: FUNCTION",
      ),
    )

    tests.foreach {
      case (input, expectedMessage) => {
        val evaluated = testEval(input)

        evaluated shouldBe a[KError]
        val errObj = evaluated.asInstanceOf[KError]
        errObj.message shouldBe expectedMessage
      }
    }
  }

  "A Evaluator" should "evaluate Let statements" in {
    val tests = List(
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    )
    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)
        testIntegerObject(evaluated, expected)
      }
    }
  }

  "A Evaluator" should "evaluate Function object" in {
    val input = "fn(x) { x + 2; };"

    val evaluated = testEval(input)
    evaluated shouldBe a[KFunction]
    val fn = evaluated.asInstanceOf[KFunction]

    val params = fn.parameters.get
    params.length shouldBe 1

    params.head.String() shouldBe "x"

    val expectedBody = "(x + 2)"
    fn.body.String() shouldBe expectedBody
  }

  "A Evaluator" should "evaluate function application" in {
    val tests = List(
      ("let identity = fn(x) { x; }; identity(5);", 5),
      ("let identity = fn(x) { return x; }; identity(5);", 5),
      ("let double = fn(x) { x * 2; }; double(5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      ("fn(x) { x; }(5)", 5),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)
        testIntegerObject(evaluated, expected)
      }
    }
  }

  "A Evaluator" should "evaluate String literal" in {
    val input = """"Hello World!""""

    val evaluated = testEval(input)
    evaluated shouldBe a[KString]

    val str = evaluated.asInstanceOf[KString]
    str.value shouldBe "Hello World!"
  }

  "A Evaluator" should "evaluate String concatenation" in {
    val input = """"Hello" + " " + "World!""""

    val evaluated = testEval(input)
    evaluated shouldBe a[KString]

    val str = evaluated.asInstanceOf[KString]
    str.value shouldBe "Hello World!"
  }

  "A Evaluator" should "evaluate builtin functions" in {
    val tests = List(
      ("""len("")""", 0),
      ("""len("four")""", 4),
      ("""len("hello world")""", 11),
      ("""len(1)""", "argument to `len` not supported, got INTEGER"),
      ("""len("one", "two")""", "wrong number of arguments. got=2, want=1"),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)

        expected match {
          case int: Int => testIntegerObject(evaluated, int)
          case _: String => {
            evaluated shouldBe a[KError]
            evaluated.asInstanceOf[KError].message shouldBe expected
          }
        }
      }
    }
  }

  "A Evaluator" should "evaluate Array literals" in {
    val input = "[1, 2 * 2, 3 + 3]"

    val evaluated = testEval(input)
    evaluated shouldBe a[KArray]

    val result = evaluated.asInstanceOf[KArray]
    result.elements.length shouldBe 3

    testIntegerObject(result.elements(0), 1)
    testIntegerObject(result.elements(1), 4)
    testIntegerObject(result.elements(2), 6)
  }

  "A Evaluator" should "evaluate Array index expressions" in {
    val tests = List(
      (
        "[1, 2, 3][0]",
        1,
      ),
      (
        "[1, 2, 3][1]",
        2,
      ),
      (
        "[1, 2, 3][2]",
        3,
      ),
      (
        "let i = 0; [1][i];",
        1,
      ),
      (
        "[1, 2, 3][1 + 1];",
        3,
      ),
      (
        "let myArray = [1, 2, 3]; myArray[2];",
        3,
      ),
      (
        "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
        6,
      ),
      (
        "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
        2,
      ),
      (
        "[1, 2, 3][3]",
        null,
      ),
      (
        "[1, 2, 3][-1]",
        null,
      ),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)

        expected match {
          case int: Int => testIntegerObject(evaluated, int)
          case _        => testNullObject(evaluated)
        }
      }
    }
  }

  "A Evaluator" should "evaluate Hash literals" in {
    val input =
      """
        |let two = "two";
        |{
        |  "one": 10 - 9,
        |  two: 1 + 1,
        |  "thr" + "ee": 6 / 2,
        |  4: 4,
        |  true: 5,
        |  false: 6
        |}
        |""".stripMargin

    val evaluated = testEval(input)
    evaluated shouldBe a[KHash]

    val result = evaluated.asInstanceOf[KHash]

    val expected = Map(
      KString("one").hashKey   -> 1,
      KString("two").hashKey   -> 2,
      KString("three").hashKey -> 3,
      KInteger(4).hashKey      -> 4,
      TRUE.hashKey             -> 5,
      FALSE.hashKey            -> 6,
    )

    result.pairs.size shouldBe expected.size

    expected.foreach {
      case (expectedKey, expectedValue) => {
        result.pairs.get(expectedKey) match {
          case Some(pair) => testIntegerObject(pair.value, expectedValue)
          case None       => fail("no pair for given key in Pairs")
        }
      }
    }
  }

  "A Evaluator" should "evaluate Hash index expressions" in {
    val tests = List(
      (
        """{"foo": 5}["foo"]""",
        5,
      ),
      (
        """{"foo": 5}["bar"]""",
        null,
      ),
      (
        """let key = "foo"; {"foo": 5}[key]""",
        5,
      ),
      (
        """{}["foo"]""",
        null,
      ),
      (
        """{5: 5}[5]""",
        5,
      ),
      (
        """{true: 5}[true]""",
        5,
      ),
      (
        """{false: 5}[false]""",
        5,
      ),
    )

    tests.foreach {
      case (input, expected) => {
        val evaluated = testEval(input)

        expected match {
          case int: Int => testIntegerObject(evaluated, int)
          case _        => testNullObject(evaluated)
        }
      }
    }
  }

  def testEval(input: String): KObject = {
    val l       = Lexer(input)
    val p       = Parser(l)
    val program = p.parseProgram()
    val env     = newEnvironment

    eval(program, env)
  }

  def testIntegerObject(obj: KObject, expected: Long): Unit = {
    obj shouldBe a[KInteger]
    val result = obj.asInstanceOf[KInteger]
    result.value shouldBe expected
  }

  def testBooleanObject(obj: KObject, expected: Boolean): Assertion = {
    obj shouldBe a[KBoolean]
    val result = obj.asInstanceOf[KBoolean]
    result.value shouldBe expected
  }

  def testNullObject(obj: KObject): Assertion = {
    obj shouldBe NULL
  }
}
