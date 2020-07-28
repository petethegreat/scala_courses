
val moose = List("cat","dog","rabbit")

moose
val moose2 = moose.map {
  case x if x.startsWith("c") => "meow"
  case x if x.startsWith("d") => "woof"
  case _ =>
}

moose2
