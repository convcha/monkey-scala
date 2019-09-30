package monkey

import monkey.repl.Repl

object Main extends App {
  val user = System.getProperty("user.name")
  println(s"Hello $user! This is the Monkey programming language!")
  println("Feel free to type in commands")
  Repl.start(System.in, System.out)
}
