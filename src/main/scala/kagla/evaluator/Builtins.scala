package kagla.evaluator

import kagla.`object`.{KArray, KBuiltin, KError, KInteger, KObject, KString}

object Builtins {
  val builtins: Map[String, KBuiltin] = Map(
    "puts" -> KBuiltin(args => {
      for (arg <- args) {
        println(arg.inspect)
      }

      Evaluator.NULL
    }),
    "len" -> KBuiltin(args => {
      if (args.length != 1) {
        KError(s"wrong number of arguments. got=${args.length}, want=1")
      } else {
        args.head match {
          case KArray(elements) => KInteger(elements.length)
          case KString(value)   => KInteger(value.length)
          case other @ _        => KError(s"argument to `len` not supported, got ${other.vtype}")
        }
      }
    }),
    "first" -> KBuiltin(args => {
      if (args.length != 1) {
        KError(s"wrong number of arguments. got=${args.length}, want=1")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        KError(s"argument to `first` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[KArray]
      if (arr.elements.nonEmpty) {
        arr.elements.head
      } else {
        Evaluator.NULL
      }
    }),
    "last" -> KBuiltin(args => {
      if (args.length != 1) {
        KError(s"wrong number of arguments. got=${args.length}, want=1")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        KError(s"argument to `last` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[KArray]
      if (arr.elements.nonEmpty) {
        arr.elements.last
      } else {
        Evaluator.NULL
      }
    }),
    "rest" -> KBuiltin(args => {
      if (args.length != 1) {
        KError(s"wrong number of arguments. got=${args.length}, want=1")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        KError(s"argument to `rest` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[KArray]
      if (arr.elements.nonEmpty) {
        KArray(arr.elements.tail)
      } else {
        Evaluator.NULL
      }
    }),
    "push" -> KBuiltin(args => {
      if (args.length != 2) {
        KError(s"wrong number of arguments. got=${args.length}, want=2")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        KError(s"argument to `push` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[KArray]
      val obj = args(1)

      KArray(arr.elements :+ obj)
    })
  )
}
