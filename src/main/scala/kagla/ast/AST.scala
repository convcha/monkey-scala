package kagla.ast

import kagla.Token

case class Program(statements: List[Statement]) {
  def tokenLiteral(): String = {
    if (statements.nonEmpty) {
      statements.head.tokenLiteral()
    } else {
      ""
    }
  }

  def String(): String = statements.foldLeft("")(_ + _.String())
}

trait Node {
  def tokenLiteral(): String
  def String(): String
}

trait Statement extends Node {
  def statementNode(): Unit
}

trait Expression extends Node {
  def expressionNode(): Unit
}

case class Identifier(token: Token, value: String) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = value
}

case class IntegerLIteral(token: Token, value: Int) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = token.literal
}

case class StringLIteral(token: Token, value: String) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = token.literal
}

case class Boolean(token: Token, value: Boolean) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = token.literal
}

case class FunctionLiteral(token: Token, parameters: List[Identifier], body: BlockStatement) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val params = parameters.map(_.String()) mkString ", "
    s"${tokenLiteral()}($params) ${body.String()}"
  }
}

case class ArrayLiteral(token: Token, elements: List[Expression]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val elems = elements.map(_.String()) mkString ", "
    s"[$elems]"
  }
}

case class HashLiteral(token: Token, pairs: Map[Expression, Expression]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val p = pairs.map { case (k, v) => s"${k.String()}:${v.String()}" } mkString ", "
    s"{$p}"
  }
}

case class LetStatement(token: Token, name: Identifier, value: Expression) extends Statement {
  override def statementNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    s"""${tokenLiteral()} ${name.String()} = ${value.String()};""" // CHANGED: if ls.Value != nil
  }
}

case class ReturnStatement(token: Token, returnValue: Expression) extends Statement {
  override def statementNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    s"""${tokenLiteral()} ${returnValue.String()};""" // CHANGED: if rs.ReturnValue != nil
  }
}

case class ExpressionStatement(token: Token, expression: Expression) extends Statement {
  override def statementNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = expression.String() // CHANGED: if es.Expression != nil
}

case class BlockStatement(token: Token, statements: List[Statement]) extends Statement {
  override def statementNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = statements.foldLeft("")(_ + _.String())
}

case class PrefixExpression(token: Token, operator: String, right: Expression) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = s"($operator${right.String()})"
}

case class InfixExpression(token: Token, left: Expression, operator: String, right: Expression) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = s"(${left.String()} $operator ${right.String()})"
}

case class IfExpression(token: Token, condition: Expression, consequence: Expression, alternative: Option[Expression])
    extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val string = s"if${condition.String()} ${consequence.String()}"

    alternative match {
      case Some(alt) => string.concat(s"else ${alt.String()}")
      case None      => string
    }
  }
}

case class CallExpression(token: Token, function: Expression, arguments: List[Expression]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val args = arguments.map(_.String()) mkString ", "
    s"${function.String()}($args)"
  }
}

case class IndexExpression(token: Token, left: Expression, index: Expression) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = s"(${left.String()}[${index.String()}])"
}
