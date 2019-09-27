package kagla.`object`

import kagla.ast.{BlockStatement, Identifier}
import kagla.`object`.KObject.{BuiltinFunction, Hashable, KObject, ObjectType}

object KObject {
  val INTEGER_OBJ      = "INTEGER"
  val STRING_OBJ       = "STRING"
  val BOOLEAN_OBJ      = "BOOLEAN"
  val NULL_OBJ         = "NULL"
  val RETURN_VALUE_OBJ = "RETURN_VALUE"
  val ERROR_OBJ        = "ERROR"
  val FUNCTION_OBJ     = "FUNCTION"
  val BUILTIN_OBJ      = "BUILTIN"
  val ARRAY_OBJ        = "ARRAY"
  val HASH_OBJ         = "HASH"

  type ObjectType      = String
  type BuiltinFunction = (KObject*) => KObject

  trait KObject {
    val vtype: ObjectType
    def inspect: String
  }

  trait Hashable {
    def hashKey: KHashKey
  }

}

case class KInteger(value: Long) extends KObject with Hashable {
  override val vtype: ObjectType = KObject.INTEGER_OBJ
  override def inspect: String   = value.toString
  override def hashKey: KHashKey = KHashKey(vtype, value)
}

case class KString(value: String) extends KObject with Hashable {
  override val vtype: ObjectType = KObject.STRING_OBJ
  override def inspect: String   = value
  override def hashKey: KHashKey = KHashKey(vtype, value.hashCode)
}

case class KBoolean(value: Boolean) extends KObject with Hashable {
  override val vtype: ObjectType = KObject.BOOLEAN_OBJ
  override def inspect: String   = value.toString
  override def hashKey: KHashKey = KHashKey(vtype, if (value) 1 else 0)
}

case class KNull() extends KObject {
  override val vtype: ObjectType = KObject.NULL_OBJ
  override def inspect: String   = "null"
}

case class KReturnValue(value: KObject) extends KObject {
  override val vtype: ObjectType = KObject.RETURN_VALUE_OBJ
  override def inspect: String   = value.inspect
}

case class KError(message: String) extends KObject {
  override val vtype: ObjectType = KObject.ERROR_OBJ
  override def inspect: String   = s"ERROR: $message"
}

case class KFunction(parameters: List[Identifier], body: BlockStatement, env: Environment) extends KObject {
  override val vtype: ObjectType = KObject.FUNCTION_OBJ
  override def inspect: String = {
    val params = parameters.map(_.String()) mkString ", "

    s"""fn($params {
       |${body.String()}
       |}
       |""".stripMargin
  }
}

case class KBuiltin(fn: BuiltinFunction) extends KObject {
  override val vtype: ObjectType = KObject.BUILTIN_OBJ
  override def inspect: String   = "builtin function"
}

case class KArray(elements: List[KObject]) extends KObject {
  override val vtype: ObjectType = KObject.ARRAY_OBJ
  override def inspect: String = {
    val elems = elements.map(_.inspect) mkString ", "
    s"[$elems]"
  }
}

case class KHashKey(vtype: ObjectType, value: Long)

case class KHashPair(key: KObject, value: KObject)

case class KHash(pairs: Map[KHashKey, KHashPair]) extends KObject {
  override val vtype: ObjectType = KObject.HASH_OBJ
  override def inspect: String = s"{${pairs.map(p => s"${p._2.key.inspect}: ${p._2.value.inspect}") mkString ", "}}"
}