package monkey.evaluator

import monkey.`object`.{MArray, MBuiltin, MError, MInteger, KObject, MString}

object Builtins {
  val builtins: Map[String, MBuiltin] = Map(
    "puts" -> MBuiltin(args => {
      for (arg <- args) {
        println(arg.inspect)
      }

      Evaluator.NULL
    }),
    "len" -> MBuiltin(args => {
      if (args.length != 1) {
        MError(s"wrong number of arguments. got=${args.length}, want=1")
      } else {
        args.head match {
          case MArray(elements) => MInteger(elements.length)
          case MString(value)   => MInteger(value.length)
          case other @ _        => MError(s"argument to `len` not supported, got ${other.vtype}")
        }
      }
    }),
    "first" -> MBuiltin(args => {
      if (args.length != 1) {
        MError(s"wrong number of arguments. got=${args.length}, want=1")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        MError(s"argument to `first` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[MArray]
      if (arr.elements.nonEmpty) {
        arr.elements.head
      } else {
        Evaluator.NULL
      }
    }),
    "last" -> MBuiltin(args => {
      if (args.length != 1) {
        MError(s"wrong number of arguments. got=${args.length}, want=1")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        MError(s"argument to `last` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[MArray]
      if (arr.elements.nonEmpty) {
        arr.elements.last
      } else {
        Evaluator.NULL
      }
    }),
    "rest" -> MBuiltin(args => {
      if (args.length != 1) {
        MError(s"wrong number of arguments. got=${args.length}, want=1")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        MError(s"argument to `rest` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[MArray]
      if (arr.elements.nonEmpty) {
        MArray(arr.elements.tail)
      } else {
        Evaluator.NULL
      }
    }),
    "push" -> MBuiltin(args => {
      if (args.length != 2) {
        MError(s"wrong number of arguments. got=${args.length}, want=2")
      }

      if (args.head.vtype != KObject.ARRAY_OBJ) {
        MError(s"argument to `push` must be ARRAY, got ${args.head.vtype}")
      }

      val arr = args.head.asInstanceOf[MArray]
      val obj = args(1)

      MArray(arr.elements :+ obj)
    })
  )
}
