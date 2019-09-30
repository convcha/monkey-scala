package kagla.`object`

import kagla.`object`.KObject.KObject

object Environment {
  def newEnvironment: Environment                             = Environment(Map())
  def newEnclosedEnvironment(outer: Environment): Environment = Environment(Map())
}

case class Environment(var store: Map[String, KObject]) {
  var outer: Environment = _

  def get(name: String): (KObject, Boolean) = {
    store.get(name) match {
      case Some(value)           => (value, true)
      case None if outer != null => outer.get(name)
      case _                     => null
    }
  }

  def set(name: String, value: KObject): KObject = {
    store = store + (name -> value)
    value
  }
}
