package monkey.ast

import monkey.token.Token

case class Program(statements: List[Statement]) extends Node {
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
  override def String(): String       = value
}

case class IntegerLIteral(token: Token, value: Long) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = token.literal
}

case class StringLIteral(token: Token, value: String) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = token.literal
}

case class Boolean(token: Token, value: scala.Boolean) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = token.literal
}

case class FunctionLiteral(token: Token, parameters: Option[List[Identifier]], body: BlockStatement)
    extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val params = parameters.getOrElse(List.empty).map(_.String()) mkString ", "
    s"${tokenLiteral()}($params) ${body.String()}"
  }
}

case class ArrayLiteral(token: Token, elements: Option[List[Option[Expression]]]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val elems = elements match {
      case Some(list) => list.flatten.map(_.String()) mkString ", "
      case None       => ""
    }
    s"[$elems]"
  }
}

case class HashLiteral(token: Token, pairs: Map[Option[Expression], Option[Expression]]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val p = pairs.map { case (k, v) => s"${k.map(_.String()).getOrElse("")}:${v.map(_.String()).getOrElse("")}" } mkString ", "
    s"{$p}"
  }
}

case class LetStatement(token: Token, name: Identifier, value: Option[Expression]) extends Statement {
  override def statementNode(): Unit  = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    s"""${tokenLiteral()} ${name.String()} = ${value.map(_.String()).getOrElse("")};""" // CHANGED: if ls.Value != nil
  }
}

case class ReturnStatement(token: Token, returnValue: Option[Expression]) extends Statement {
  override def statementNode(): Unit  = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    s"""${tokenLiteral()} ${returnValue.map(_.String()).getOrElse("")};""" // CHANGED: if rs.ReturnValue != nil
  }
}

case class ExpressionStatement(token: Token, expression: Option[Expression]) extends Statement {
  override def statementNode(): Unit  = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = expression.map(_.String()).getOrElse("") // CHANGED: if es.Expression != nil
}

case class BlockStatement(token: Token, statements: List[Statement]) extends Statement {
  override def statementNode(): Unit  = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = statements.foldLeft("")(_ + _.String())
}

case class PrefixExpression(token: Token, operator: String, right: Option[Expression]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = s"($operator${right.map(_.String()).getOrElse("")})"
}

case class InfixExpression(token: Token, left: Expression, operator: String, right: Option[Expression])
    extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = s"(${left.String()} $operator ${right.map(_.String()).getOrElse("")})"
}

case class IfExpression(token: Token,
                        condition: Option[Expression],
                        consequence: BlockStatement,
                        alternative: Option[BlockStatement])
    extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val string = s"if${condition.map(_.String()).getOrElse("")} ${consequence.String()}"

    alternative match {
      case Some(alt) => string.concat(s"else ${alt.String()}")
      case None      => string
    }
  }
}

case class CallExpression(token: Token, function: Expression, arguments: Option[List[Option[Expression]]])
    extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String = {
    val args = arguments match {
      case Some(a) => a.map(_.map(_.String()).getOrElse("")) mkString ", "
      case None    => ""
    }
    s"${function.String()}($args)"
  }
}

case class IndexExpression(token: Token, left: Expression, index: Option[Expression]) extends Expression {
  override def expressionNode(): Unit = {}
  override def tokenLiteral(): String = token.literal
  override def String(): String       = s"(${left.String()}[${index.map(_.String()).getOrElse("")}])"
}
