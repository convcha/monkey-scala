package monkey.`object`

import monkey.`object`.KObject.MObject

object Environment {
  def newEnvironment: Environment                             = Environment(Map())
  def newEnclosedEnvironment(outer: Environment): Environment = Environment(Map())
}

case class Environment(var store: Map[String, MObject]) {
  var outer: Environment = _

  def get(name: String): (MObject, Boolean) = {
    store.get(name) match {
      case Some(value)           => (value, true)
      case None if outer != null => outer.get(name)
      case _                     => null
    }
  }

  def set(name: String, value: MObject): MObject = {
    store = store + (name -> value)
    value
  }
}
