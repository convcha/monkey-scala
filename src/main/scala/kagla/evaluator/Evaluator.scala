package kagla.evaluator

import kagla.`object`.KObject.{Hashable, KObject}
import kagla.`object`._
import kagla.ast
import kagla.ast.{
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
import kagla.`object`.Environment._
import kagla.evaluator.Builtins._

object Evaluator {
  val NULL  = KNull()
  val TRUE  = KBoolean(true)
  val FALSE = KBoolean(false)

  def eval(node: Node, env: Environment): KObject = node match {
    case program: Program => evalProgram(program, env)
    case expressionStatement: ExpressionStatement =>
      eval(expressionStatement.expression.get, env) // FIXME: expressionStatement.expression.get
    case integerLiteral: IntegerLIteral   => KInteger(integerLiteral.value)
    case stringLIteral: StringLIteral     => KString(stringLIteral.value)
    case boolean: ast.Boolean             => nativeBoolToBooleanObject(boolean.value)
    case functionLiteral: FunctionLiteral => KFunction(functionLiteral.parameters, functionLiteral.body, env)
    case arrayLiteral: ArrayLiteral =>
      evalExpressions(arrayLiteral.elements.get.flatten, env) match { // FIXME: arrayLiteral.elements.get.flatten
        case elements if elements.length == 1 && isError(elements.head) => elements.head
        case elements                                                   => KArray(elements)
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
        case value                   => KReturnValue(value)
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

  def evalProgram(program: Program, env: Environment): KObject = {
    var result: KObject = null

    for (statement <- program.statements) {
      result = eval(statement, env)

      result match {
        case KReturnValue(v) => return v
        case e @ KError(_)   => return e
        case r => result = r
      }
    }

    result
  }

  def evalPrefixExpression(operator: String, right: KObject): KObject = operator match {
    case "!" => evalBangOperatorExpression(right)
    case "-" => evalMinusPrefixOperatorExpression(right)
    case _   => KError(s"unknown operator: $operator${right.vtype}")
  }

  def evalBangOperatorExpression(right: KObject): KObject = right match {
    case TRUE  => FALSE
    case FALSE => TRUE
    case NULL  => TRUE
    case _     => FALSE
  }

  def evalMinusPrefixOperatorExpression(right: KObject): KObject = {
    if (right.vtype != KObject.INTEGER_OBJ) {
      KError(s"unknown operator: -${right.vtype}")
    } else {
      val value = right.asInstanceOf[KInteger].value
      KInteger(-value)
    }
  }

  def evalInfixExpression(operator: String, left: KObject, right: KObject): KObject = {
    if (left.vtype == KObject.INTEGER_OBJ && right.vtype == KObject.INTEGER_OBJ) {
      evalIntegerInfixExpression(operator, left, right)
    } else if (left.vtype == KObject.STRING_OBJ && right.vtype == KObject.STRING_OBJ) {
      evalStringInfixExpression(operator, left, right)
    } else if (operator == "==") {
      nativeBoolToBooleanObject(left == right)
    } else if (operator == "!=") {
      nativeBoolToBooleanObject(left != right)
    } else if (left.vtype != right.vtype) {
      KError(s"type mismatch: ${left.vtype} $operator ${right.vtype}")
    } else {
      KError(s"unknown operator: ${left.vtype} $operator ${right.vtype}")
    }
  }

  def evalIndexExpression(left: KObject, index: KObject): KObject = {
    if (left.vtype == KObject.ARRAY_OBJ && index.vtype == KObject.INTEGER_OBJ) {
      evalArrayIndexExpression(left, index)
    } else if (left.vtype == KObject.HASH_OBJ) {
      evalHashIndexExpression(left, index)
    } else {
      KError(s"index operator not supported: ${left.vtype}")
    }
  }

  def evalIntegerInfixExpression(operator: String, left: KObject, right: KObject): KObject = {
    val leftVal  = left.asInstanceOf[KInteger].value
    val rightVal = right.asInstanceOf[KInteger].value

    operator match {
      case "+"  => KInteger(leftVal + rightVal)
      case "-"  => KInteger(leftVal - rightVal)
      case "*"  => KInteger(leftVal * rightVal)
      case "/"  => KInteger(leftVal / rightVal)
      case "<"  => nativeBoolToBooleanObject(leftVal < rightVal)
      case ">"  => nativeBoolToBooleanObject(leftVal > rightVal)
      case "==" => nativeBoolToBooleanObject(leftVal == rightVal)
      case "!=" => nativeBoolToBooleanObject(leftVal != rightVal)
      case _    => KError(s"unknown operator: ${left.vtype} $operator ${right.vtype}")
    }
  }

  def evalStringInfixExpression(operator: String, left: KObject, right: KObject): KObject = {
    if (operator != "+") {
      return KError(s"unknown operator: ${left.vtype} $operator ${right.vtype}")
    }

    val leftVal  = left.asInstanceOf[KString].value
    val rightVal = right.asInstanceOf[KString].value
    KString(leftVal + rightVal)
  }

  def nativeBoolToBooleanObject(input: Boolean): KBoolean = if (input) TRUE else FALSE

  def evalIfExpression(ie: IfExpression, env: Environment): KObject = {
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

  def evalBlockStatement(block: BlockStatement, env: Environment): KObject = {
    var result: KObject = null

    for (statement <- block.statements) {
      result = eval(statement, env)

      if (result != null && (result.vtype == KObject.RETURN_VALUE_OBJ || result.vtype == KObject.ERROR_OBJ)) {
        return result
      }
    }

    result
  }

  def evalIdentifier(node: Identifier, env: Environment): KObject = {
    env.get(node.value) match {
      case (value, true) => value
      case _ =>
        builtins.get(node.value) match {
          case Some(value) => value
          case None        => KError(s"identifier not found: ${node.value}")
        }
    }
  }

  def evalExpressions(exps: List[Expression], env: Environment): List[KObject] = {
    var result = List.empty[KObject]

    for (e <- exps) {
      val evaluated = eval(e, env)

      if (isError(evaluated)) {
        return List(evaluated)
      }

      result = result :+ evaluated
    }

    result
  }

  def isTruthy(obj: KObject): Boolean = obj match {
    case NULL  => false
    case TRUE  => true
    case FALSE => false
    case _     => true
  }

  def isError(obj: KObject): Boolean = {
    if (obj != null) {
      return obj.vtype == KObject.ERROR_OBJ
    }

    false
  }

  def applyFunction(fn: KObject, args: List[KObject]): KObject = fn match {
    case function: KFunction => {
      val extendedEnv = extendFunctionEnv(function, args)
      val evaluated   = eval(function.body, extendedEnv)
      unwrapReturnValue(evaluated)
    }
    case builtin: KBuiltin => builtin.fn(args: _*)
    case _                 => KError(f"not a function: ${fn.vtype}")
  }

  def extendFunctionEnv(fn: KFunction, args: List[KObject]): Environment = {
    val env = newEnclosedEnvironment(fn.env)

    fn.parameters.foreach(_.zipWithIndex.foreach {
      case (param: Identifier, paramIdx: Int) => env.set(param.value, args(paramIdx))
    })

    env
  }

  def unwrapReturnValue(obj: KObject): KObject = obj match {
    case KReturnValue(value) => value
    case _                   => obj
  }

  def evalArrayIndexExpression(array: KObject, index: KObject): KObject = {
    val arrayObject = array.asInstanceOf[KArray]
    val idx         = index.asInstanceOf[KInteger].value
    val max         = arrayObject.elements.length - 1

    if (idx < 0 || idx > max) {
      return NULL
    }

    arrayObject.elements(idx.toInt)
  }

  def evalHashIndexExpression(hash: KObject, index: KObject): KObject = {
    val hashObject = hash.asInstanceOf[KHash]

    index match {
      case key: Hashable =>
        hashObject.pairs.get(key.hashKey) match {
          case Some(pair) => pair.value
          case _          => NULL
        }
      case _ => KError(s"unusable as hash key: ${index.vtype}")
    }
  }

  def evalHashLiteral(node: HashLiteral, env: Environment): KObject = {
    var pairs = Map[KHashKey, KHashPair]()

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
            pairs = pairs + (hashed -> KHashPair(key, value))
          }
          case _ => KError(s"unusable as hash key: ${key.vtype}")
        }
      }
    }

    KHash(pairs)
  }
}
