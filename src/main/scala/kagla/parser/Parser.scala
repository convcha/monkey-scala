package kagla.parser

import kagla.parser.Parser._
import kagla.ast
import kagla.token.Token._
import kagla.ast.{ArrayLiteral, BlockStatement, CallExpression, Expression, ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLIteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement, StringLIteral}
import kagla.lexer.Lexer

object Parser {
  type PrefixParseFn = () => Option[Expression]
  type InfixParseFn  = Expression => Option[Expression]

  object Precedence extends Enumeration {
    val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL, INDEX = Value
  }

  val precedences: Map[TokenType, Precedence.Value] = Map(
    EQ       -> Precedence.EQUALS,
    NOT_EQ   -> Precedence.EQUALS,
    LT       -> Precedence.LESSGREATER,
    GT       -> Precedence.LESSGREATER,
    PLUS     -> Precedence.SUM,
    MINUS    -> Precedence.SUM,
    SLASH    -> Precedence.PRODUCT,
    ASTERISK -> Precedence.PRODUCT,
    LPAREN   -> Precedence.CALL,
    LBRACKET -> Precedence.INDEX,
  )
}

case class Parser(l: Lexer) {

  private var curToken  = l.nextToken()
  private var peekToken = l.nextToken()
  var errors            = List.empty[String]

  private val prefixParseFns: Map[TokenType, PrefixParseFn] = Map(
    IDENT    -> parseIdentifier,
    INT      -> parseIntegerLiteral,
    BANG     -> parsePrefixExpression,
    MINUS    -> parsePrefixExpression,
    TRUE     -> parseBoolean,
    FALSE    -> parseBoolean,
    LPAREN   -> parseGroupedExpression,
    IF       -> parseIfExpression,
    FUNCTION -> parseFunctionLiteral,
    STRING   -> parseStringLiteral,
    LBRACKET -> parseArrayLiteral,
    LBRACE   -> parseHashLiteral,
  )
  private val infixParseFns: Map[TokenType, InfixParseFn] = Map(
    PLUS     -> parseInfixExpression,
    MINUS    -> parseInfixExpression,
    SLASH    -> parseInfixExpression,
    ASTERISK -> parseInfixExpression,
    EQ       -> parseInfixExpression,
    NOT_EQ   -> parseInfixExpression,
    LT       -> parseInfixExpression,
    GT       -> parseInfixExpression,
    LPAREN   -> parseCallExpression,
    LBRACKET -> parseIndexExpression,
  )

  def nextToken(): Unit = {
    curToken = peekToken
    peekToken = l.nextToken()
  }

  def parseProgram(): Program = {
    var statements = List.empty[Statement]

    while (curToken.tokenType != EOF) {
      val stmt = parseStatement()
      stmt.foreach { s =>
        statements = statements :+ s
      }
      nextToken()
    }

    Program(statements)
  }

  def parseStatement(): Option[Statement] = curToken.tokenType match {
    case LET    => parseLetStatement()
    case RETURN => parseReturnStatement()
    case _      => parseExpressionStatement()
  }

  def parseExpression(precedence: Precedence.Value): Option[Expression] = {
    var leftExp = for {
      prefix  <- prefixParseFns.get(curToken.tokenType)
      leftExp <- prefix()
    } yield leftExp

    if (leftExp.isEmpty) {
      noPrefixParseFnError(curToken.tokenType)
      return None
    }

    while (!peekTokenIs(SEMICOLON) && precedence < peekPrecedence()) {
      infixParseFns.get(peekToken.tokenType) match {
        case Some(infix) =>
          nextToken()
          leftExp = infix(leftExp.get)
        case None => return leftExp
      }
    }

    leftExp
  }

  def parseLetStatement(): Option[LetStatement] = {
    val stmtToken = curToken

    if (!expectPeek(IDENT)) {
      return None
    }

    val name = Identifier(token = curToken, value = curToken.literal)

    if (!expectPeek(ASSIGN)) {
      return None
    }

    nextToken()

    val value = parseExpression(Precedence.LOWEST)

    if (peekTokenIs(SEMICOLON)) {
      nextToken()
    }

    Some(LetStatement(stmtToken, name, value))
  }

  def parseReturnStatement(): Option[ReturnStatement] = {
    val stmtToken = curToken

    nextToken()

    val returnValue = parseExpression(Precedence.LOWEST)

    if (peekTokenIs(SEMICOLON)) {
      nextToken()
    }

    Some(ReturnStatement(stmtToken, returnValue))
  }

  def parseExpressionStatement(): Option[ExpressionStatement] = {
    val stmtToken = curToken

    val returnValue = parseExpression(Precedence.LOWEST)

    if (peekTokenIs(SEMICOLON)) {
      nextToken()
    }

    Some(ExpressionStatement(stmtToken, returnValue))
  }

  def parseBlockStatement(): BlockStatement = {
    val cur = curToken

    nextToken()

    var statements = List.empty[Statement]
    while (!curTokenIs(RBRACE) && !curTokenIs(EOF)) {
      val stmt = parseStatement()
      stmt.foreach(s => statements = statements :+ s)
      nextToken()
    }

    BlockStatement(cur, statements)
  }

  def parseExpressionList(end: TokenType): Option[List[Option[Expression]]] = { // FIXME
    if (peekTokenIs(end)) {
      nextToken()
      return Some(List.empty[Option[Expression]])
    }

    nextToken()
    var list = List(parseExpression(Precedence.LOWEST))

    while (peekTokenIs(COMMA)) {
      nextToken()
      nextToken()
      list = list :+ parseExpression(Precedence.LOWEST)
    }

    if (!expectPeek(end)) {
      None
    }

    Some(list)
  }

  def parseInfixExpression(left: Expression): Option[Expression] = {
    val expToken = curToken
    val expOp    = curToken.literal

    val precedence = curPrecedence()
    nextToken()

    Some(InfixExpression(expToken, left, expOp, parseExpression(precedence)))
  }

  def curTokenIs(t: TokenType): Boolean = curToken.tokenType == t

  def peekTokenIs(t: TokenType): Boolean = peekToken.tokenType == t

  def expectPeek(t: TokenType): Boolean =
    if (peekTokenIs(t)) {
      nextToken()
      true
    } else {
      peekError(t)
      false
    }

  def peekError(t: TokenType): Unit = {
    val msg = s"expected next token to be $t, got ${peekToken.tokenType} instead"
    errors = errors :+ msg
  }

  def parseIdentifier(): Option[Expression] = Some(Identifier(token = curToken, value = curToken.literal))

  def parseIntegerLiteral(): Option[Expression] = {
    try {
      val lit = curToken.literal.toLong
      Some(IntegerLIteral(curToken, lit))
    } catch {
      case _: NumberFormatException => {
        val msg = s"could not parse ${curToken.literal} as integer"
        errors = errors :+ msg
        None
      }
    }
  }

  def parseStringLiteral(): Option[Expression] = Some(StringLIteral(curToken, curToken.literal))

  def parsePrefixExpression(): Option[Expression] = {
    val token    = curToken
    val operator = curToken.literal

    nextToken()
    val right = parseExpression(Precedence.PREFIX)

    Some(PrefixExpression(token, operator, right))
  }

  def parseArrayLiteral(): Option[Expression] = Some(ArrayLiteral(curToken, parseExpressionList(RBRACKET)))

  def parseHashLiteral(): Option[Expression] = {
    val cur   = curToken
    var pairs = Map[Option[Expression], Option[Expression]]() // FIXME: Stop using Option!

    while (!peekTokenIs(RBRACE)) {
      nextToken()
      val key = parseExpression(Precedence.LOWEST)
      if (!expectPeek(COLON)) {
        return None
      }

      nextToken()
      val value = parseExpression(Precedence.LOWEST)

      pairs = pairs.updated(key, value)

      if (!peekTokenIs(RBRACE) && !expectPeek(COMMA)) {
        return None
      }
    }

    if (!expectPeek(RBRACE)) {
      return None
    }

    Some(HashLiteral(cur, pairs))
  }

  def parseFunctionLiteral(): Option[Expression] = {
    val cur = curToken

    if (!expectPeek(LPAREN)) {
      return None
    }

    val parameters = parseFunctionParameters()

    if (!expectPeek(LBRACE)) {
      return None
    }

    Some(FunctionLiteral(cur, parameters, parseBlockStatement()))
  }

  def parseFunctionParameters(): Option[List[Identifier]] = {
    var identifiers = List.empty[Identifier]

    if (peekTokenIs(RPAREN)) {
      nextToken()
      return Some(identifiers)
    }

    nextToken()

    identifiers = identifiers :+ Identifier(curToken, curToken.literal)

    while (peekTokenIs(COMMA)) {
      nextToken()
      nextToken()
      identifiers = identifiers :+ Identifier(curToken, curToken.literal)
    }

    if (!expectPeek(RPAREN)) {
      return None
    }

    Some(identifiers)
  }

  def parseBoolean(): Option[Expression] = Some(ast.Boolean(curToken, curTokenIs(TRUE)))

  def parseGroupedExpression(): Option[Expression] = {
    nextToken()

    val exp = parseExpression(Precedence.LOWEST)

    if (!expectPeek(RPAREN)) {
      return None
    }

    exp
  }

  def parseIfExpression(): Option[Expression] = {
    val cur = curToken

    if (!expectPeek(LPAREN)) {
      return None
    }

    nextToken()
    val condition = parseExpression(Precedence.LOWEST)

    if (!expectPeek(RPAREN)) {
      return None
    }

    if (!expectPeek(LBRACE)) {
      return None
    }

    val consequence                         = parseBlockStatement()
    var alternative: Option[BlockStatement] = None

    if (peekTokenIs(ELSE)) {
      nextToken()

      if (!expectPeek(LBRACE)) {
        return None
      }

      alternative = Some(parseBlockStatement())
    }

    Some(IfExpression(cur, condition, consequence, alternative))
  }

  def parseCallExpression(function: Expression): Option[Expression] =
    Some(CallExpression(curToken, function, parseExpressionList(RPAREN)))

  def parseIndexExpression(left: Expression): Option[Expression] = {
    val cur = curToken

    nextToken()
    val index = parseExpression(Precedence.LOWEST)

    if (!expectPeek(RBRACKET)) {
      return None
    }

    Some(IndexExpression(cur, left, index))
  }

  def peekPrecedence(): Precedence.Value = precedences.get(peekToken.tokenType) match {
    case Some(p) => p
    case None    => Precedence.LOWEST
  }

  def curPrecedence(): Precedence.Value = precedences.get(curToken.tokenType) match {
    case Some(p) => p
    case None    => Precedence.LOWEST
  }

  def noPrefixParseFnError(t: TokenType): Unit = {
    val msg = s"no prefix parse function for $t found"
    errors = errors :+ msg
  }
}
