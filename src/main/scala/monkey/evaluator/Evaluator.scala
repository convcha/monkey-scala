package monkey.evaluator

import monkey.`object`.KObject.{Hashable, MObject}
import monkey.`object`._
import monkey.ast
import monkey.ast.{
  ArrayLiteral,
  BlockStatement,
  CallExpression,
  Expression,
  ExpressionStatement,
  FunctionLiteral,
  HashLiteral,
  Identifier,
  IfExpression,
  IndexExpression,
  InfixExpression,
  IntegerLIteral,
  LetStatement,
  Node,
  PrefixExpression,
  Program,
  ReturnStatement,
  StringLIteral
}
import monkey.`object`.Environment._
import monkey.evaluator.Builtins._

object Evaluator {
  val NULL  = MNull()
  val TRUE  = MBoolean(true)
  val FALSE = MBoolean(false)

  def eval(node: Node, env: Environment): MObject = node match {
    case program: Program => evalProgram(program, env)
    case expressionStatement: ExpressionStatement =>
      eval(expressionStatement.expression.get, env) // FIXME: expressionStatement.expression.get
    case integerLiteral: IntegerLIteral   => MInteger(integerLiteral.value)
    case stringLIteral: StringLIteral     => MString(stringLIteral.value)
    case boolean: ast.Boolean             => nativeBoolToBooleanObject(boolean.value)
    case functionLiteral: FunctionLiteral => MFunction(functionLiteral.parameters, functionLiteral.body, env)
    case arrayLiteral: ArrayLiteral =>
      evalExpressions(arrayLiteral.elements.get.flatten, env) match { // FIXME: arrayLiteral.elements.get.flatten
        case elements if elements.length == 1 && isError(elements.head) => elements.head
        case elements                                                   => MArray(elements)
      }
    case hashLiteral: HashLiteral => evalHashLiteral(hashLiteral, env)
    // FIXME: prefixExpression.right.get
    case prefixExpression: PrefixExpression =>
      eval(prefixExpression.right.get, env) match {
        case right if isError(right) => right
        case right                   => evalPrefixExpression(prefixExpression.operator, right)
      }
    case infixExpression: InfixExpression =>
      eval(infixExpression.left, env) match {
        case left if isError(left) => left
        // FIXME: infixExpression.right.get
        case left =>
          eval(infixExpression.right.get, env) match {
            case right if isError(right) => right
            case right                   => evalInfixExpression(infixExpression.operator, left, right)
          }
      }
    case indexExpression: IndexExpression =>
      eval(indexExpression.left, env) match {
        case left if isError(left) => left
        // FIXME: indexExpression.index.get
        case left =>
          eval(indexExpression.index.get, env) match {
            case index if isError(index) => index
            case index                   => evalIndexExpression(left, index)
          }
      }
    case blockStatement: BlockStatement => evalBlockStatement(blockStatement, env)
    case ifExpression: IfExpression     => evalIfExpression(ifExpression, env)
    case callExpression: CallExpression =>
      eval(callExpression.function, env) match {
        case function if isError(function) => function
        // FIXME: indexExpression.index.get
        case function =>
          evalExpressions(callExpression.arguments.get.flatten, env) match { // FIXME: callExpression.arguments.get.flatten
            case args if args.length == 1 && isError(args.head) => args.head
            case args                                           => applyFunction(function, args)
          }
      }
    // FIXME: returnStatement.returnValue.get
    case returnStatement: ReturnStatement =>
      eval(returnStatement.returnValue.get, env) match {
        case value if isError(value) => value
        case value                   => MReturnValue(value)
      }
    // FIXME: letStatement.value.get
    case letStatement: LetStatement =>
      eval(letStatement.value.get, env) match {
        case value if isError(value) => value
        case value                   => env.set(letStatement.name.value, value)
      }
    case identifier: Identifier => evalIdentifier(identifier, env)
    case _                      => null
  }

  def evalProgram(program: Program, env: Environment): MObject = {
    var result: MObject = null

    for (statement <- program.statements) {
      result = eval(statement, env)

      result match {
        case MReturnValue(v) => return v
        case e @ MError(_)   => return e
        case r => result = r
      }
    }

    result
  }

  def evalPrefixExpression(operator: String, right: MObject): MObject = operator match {
    case "!" => evalBangOperatorExpression(right)
    case "-" => evalMinusPrefixOperatorExpression(right)
    case _   => MError(s"unknown operator: $operator${right.vtype}")
  }

  def evalBangOperatorExpression(right: MObject): MObject = right match {
    case TRUE  => FALSE
    case FALSE => TRUE
    case NULL  => TRUE
    case _     => FALSE
  }

  def evalMinusPrefixOperatorExpression(right: MObject): MObject = {
    if (right.vtype != KObject.INTEGER_OBJ) {
      MError(s"unknown operator: -${right.vtype}")
    } else {
      val value = right.asInstanceOf[MInteger].value
      MInteger(-value)
    }
  }

  def evalInfixExpression(operator: String, left: MObject, right: MObject): MObject = {
    if (left.vtype == KObject.INTEGER_OBJ && right.vtype == KObject.INTEGER_OBJ) {
      evalIntegerInfixExpression(operator, left, right)
    } else if (left.vtype == KObject.STRING_OBJ && right.vtype == KObject.STRING_OBJ) {
      evalStringInfixExpression(operator, left, right)
    } else if (operator == "==") {
      nativeBoolToBooleanObject(left == right)
    } else if (operator == "!=") {
      nativeBoolToBooleanObject(left != right)
    } else if (left.vtype != right.vtype) {
      MError(s"type mismatch: ${left.vtype} $operator ${right.vtype}")
    } else {
      MError(s"unknown operator: ${left.vtype} $operator ${right.vtype}")
    }
  }

  def evalIndexExpression(left: MObject, index: MObject): MObject = {
    if (left.vtype == KObject.ARRAY_OBJ && index.vtype == KObject.INTEGER_OBJ) {
      evalArrayIndexExpression(left, index)
    } else if (left.vtype == KObject.HASH_OBJ) {
      evalHashIndexExpression(left, index)
    } else {
      MError(s"index operator not supported: ${left.vtype}")
    }
  }

  def evalIntegerInfixExpression(operator: String, left: MObject, right: MObject): MObject = {
    val leftVal  = left.asInstanceOf[MInteger].value
    val rightVal = right.asInstanceOf[MInteger].value

    operator match {
      case "+"  => MInteger(leftVal + rightVal)
      case "-"  => MInteger(leftVal - rightVal)
      case "*"  => MInteger(leftVal * rightVal)
      case "/"  => MInteger(leftVal / rightVal)
      case "<"  => nativeBoolToBooleanObject(leftVal < rightVal)
      case ">"  => nativeBoolToBooleanObject(leftVal > rightVal)
      case "==" => nativeBoolToBooleanObject(leftVal == rightVal)
      case "!=" => nativeBoolToBooleanObject(leftVal != rightVal)
      case _    => MError(s"unknown operator: ${left.vtype} $operator ${right.vtype}")
    }
  }

  def evalStringInfixExpression(operator: String, left: MObject, right: MObject): MObject = {
    if (operator != "+") {
      return MError(s"unknown operator: ${left.vtype} $operator ${right.vtype}")
    }

    val leftVal  = left.asInstanceOf[MString].value
    val rightVal = right.asInstanceOf[MString].value
    MString(leftVal + rightVal)
  }

  def nativeBoolToBooleanObject(input: Boolean): MBoolean = if (input) TRUE else FALSE

  def evalIfExpression(ie: IfExpression, env: Environment): MObject = {
    // FIXME: ie.condition.get
    val condition = eval(ie.condition.get, env)

    if (isError(condition)) {
      return condition
    }

    if (isTruthy(condition)) {
      eval(ie.consequence, env)
    } else if (ie.alternative.nonEmpty) {
      eval(ie.alternative.get, env)
    } else {
      NULL
    }
  }

  def evalBlockStatement(block: BlockStatement, env: Environment): MObject = {
    var result: MObject = null

    for (statement <- block.statements) {
      result = eval(statement, env)

      if (result != null && (result.vtype == KObject.RETURN_VALUE_OBJ || result.vtype == KObject.ERROR_OBJ)) {
        return result
      }
    }

    result
  }

  def evalIdentifier(node: Identifier, env: Environment): MObject = {
    env.get(node.value) match {
      case (value, true) => value
      case _ =>
        builtins.get(node.value) match {
          case Some(value) => value
          case None        => MError(s"identifier not found: ${node.value}")
        }
    }
  }

  def evalExpressions(exps: List[Expression], env: Environment): List[MObject] = {
    var result = List.empty[MObject]

    for (e <- exps) {
      val evaluated = eval(e, env)

      if (isError(evaluated)) {
        return List(evaluated)
      }

      result = result :+ evaluated
    }

    result
  }

  def isTruthy(obj: MObject): Boolean = obj match {
    case NULL  => false
    case TRUE  => true
    case FALSE => false
    case _     => true
  }

  def isError(obj: MObject): Boolean = {
    if (obj != null) {
      return obj.vtype == KObject.ERROR_OBJ
    }

    false
  }

  def applyFunction(fn: MObject, args: List[MObject]): MObject = fn match {
    case function: MFunction => {
      val extendedEnv = extendFunctionEnv(function, args)
      val evaluated   = eval(function.body, extendedEnv)
      unwrapReturnValue(evaluated)
    }
    case builtin: MBuiltin => builtin.fn(args: _*)
    case _                 => MError(f"not a function: ${fn.vtype}")
  }

  def extendFunctionEnv(fn: MFunction, args: List[MObject]): Environment = {
    val env = newEnclosedEnvironment(fn.env)

    fn.parameters.foreach(_.zipWithIndex.foreach {
      case (param: Identifier, paramIdx: Int) => env.set(param.value, args(paramIdx))
    })

    env
  }

  def unwrapReturnValue(obj: MObject): MObject = obj match {
    case MReturnValue(value) => value
    case _                   => obj
  }

  def evalArrayIndexExpression(array: MObject, index: MObject): MObject = {
    val arrayObject = array.asInstanceOf[MArray]
    val idx         = index.asInstanceOf[MInteger].value
    val max         = arrayObject.elements.length - 1

    if (idx < 0 || idx > max) {
      return NULL
    }

    arrayObject.elements(idx.toInt)
  }

  def evalHashIndexExpression(hash: MObject, index: MObject): MObject = {
    val hashObject = hash.asInstanceOf[MHash]

    index match {
      case key: Hashable =>
        hashObject.pairs.get(key.hashKey) match {
          case Some(pair) => pair.value
          case _          => NULL
        }
      case _ => MError(s"unusable as hash key: ${index.vtype}")
    }
  }

  def evalHashLiteral(node: HashLiteral, env: Environment): MObject = {
    var pairs = Map[MHashKey, MHashPair]()

    node.pairs.foreach {
      case (keyNode, valueNode) => {
        val key = eval(keyNode.get, env) // FIXME: keyNode.get
        if (isError(key)) {
          return key
        }

        key match {
          case hashKey: Hashable => {
            val value = eval(valueNode.get, env) // FIXME: valueNode.get
            if (isError(value)) {
              return value
            }

            val hashed = hashKey.hashKey
            pairs = pairs + (hashed -> MHashPair(key, value))
          }
          case _ => MError(s"unusable as hash key: ${key.vtype}")
        }
      }
    }

    MHash(pairs)
  }
}
