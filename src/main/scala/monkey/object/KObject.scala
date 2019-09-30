package monkey.`object`

import monkey.ast.{BlockStatement, Identifier}
import monkey.`object`.KObject.{BuiltinFunction, Hashable, MObject, ObjectType}

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
  type BuiltinFunction = (MObject*) => MObject

  trait MObject {
    val vtype: ObjectType
    def inspect: String
  }

  trait Hashable {
    def hashKey: MHashKey
  }

}

case class MInteger(value: Long) extends MObject with Hashable {
  override val vtype: ObjectType = KObject.INTEGER_OBJ
  override def inspect: String   = value.toString
  override def hashKey: MHashKey = MHashKey(vtype, value)
}

case class MString(value: String) extends MObject with Hashable {
  override val vtype: ObjectType = KObject.STRING_OBJ
  override def inspect: String   = value
  override def hashKey: MHashKey = MHashKey(vtype, value.hashCode)
}

case class MBoolean(value: Boolean) extends MObject with Hashable {
  override val vtype: ObjectType = KObject.BOOLEAN_OBJ
  override def inspect: String   = value.toString
  override def hashKey: MHashKey = MHashKey(vtype, if (value) 1 else 0)
}

case class MNull() extends MObject {
  override val vtype: ObjectType = KObject.NULL_OBJ
  override def inspect: String   = "null"
}

case class MReturnValue(value: MObject) extends MObject {
  override val vtype: ObjectType = KObject.RETURN_VALUE_OBJ
  override def inspect: String   = value.inspect
}

case class MError(message: String) extends MObject {
  override val vtype: ObjectType = KObject.ERROR_OBJ
  override def inspect: String   = s"ERROR: $message"
}

case class MFunction(parameters: Option[List[Identifier]], body: BlockStatement, env: Environment) extends MObject {
  override val vtype: ObjectType = KObject.FUNCTION_OBJ
  override def inspect: String = {
    val params = parameters.getOrElse(List.empty[Identifier]).map(_.String()) mkString ", "

    s"""fn($params) {
       |${body.String()}
       |}
       |""".stripMargin
  }
}

case class MBuiltin(fn: BuiltinFunction) extends MObject {
  override val vtype: ObjectType = KObject.BUILTIN_OBJ
  override def inspect: String   = "builtin function"
}

case class MArray(elements: List[MObject]) extends MObject {
  override val vtype: ObjectType = KObject.ARRAY_OBJ
  override def inspect: String = {
    val elems = elements.map(_.inspect) mkString ", "
    s"[$elems]"
  }
}

case class MHashKey(vtype: ObjectType, value: Long)

case class MHashPair(key: MObject, value: MObject)

case class MHash(pairs: Map[MHashKey, MHashPair]) extends MObject {
  override val vtype: ObjectType = KObject.HASH_OBJ
  override def inspect: String = s"{${pairs.map(p => s"${p._2.key.inspect}: ${p._2.value.inspect}") mkString ", "}}"
}