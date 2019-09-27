package kagla.`object`

import kagla.`object`.KObject.KObject

case class Environment(store: Map[String, KObject], outer: Environment)
