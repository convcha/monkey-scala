package kagla.repl

import java.io.{InputStream, PrintStream}
import java.util.Scanner

import kagla.`object`.Environment._
import kagla.evaluator.Evaluator._
import kagla.lexer.Lexer
import kagla.parser.Parser

object Repl {
  val PROMPT = ">> "

  val MONKEY_FACE = """
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-*******-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
  """

  def start(in: InputStream, out: PrintStream): Unit = {
    val scanner = new Scanner(in)
    val env     = newEnvironment

    while (true) {
      out.print(PROMPT)
      val line = scanner.nextLine()

      val l = Lexer(line)
      val p = Parser(l)

      val program = p.parseProgram()
      if (p.errors.nonEmpty) {
        printParserErrors(out, p.errors)
      } else {
        val evaluated = eval(program, env)
        if (evaluated != null) {
          out.print(evaluated.inspect)
          out.print("\n")
        }
      }
    }
  }

  def printParserErrors(out: PrintStream, errors: List[String]): Unit = {
    out.print(MONKEY_FACE)
    out.print("Woops! We ran into some monkey business here!\n")
    out.print(" parser errors:\n")
    for (msg <- errors) {
      out.print(s"\t$msg\n")
    }
  }
}
