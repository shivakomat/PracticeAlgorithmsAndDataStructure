package common

object ConvertTextIntoHtmlText extends App {

  val str = "Hello world"
  val applyActions = Seq(("B", (6, 7)), ("U", (9, 10)), ("I", (1, 3)))

  val r = str.toCharArray.zipWithIndex.map(c => {
    val startingAction = applyActions.find(e => c._2 == e._2._1)
    val endAction = applyActions.find(e => c._2 == e._2._2)
    if(startingAction.nonEmpty) {
      val open = startingAction.get._1 match {
        case "B" => "<B>"
        case "U" => "<U>"
        case "I" => "<I>"
      }
      open.concat(c._1.toString)

    } else if(endAction.nonEmpty) {
      val close = endAction.get._1 match {
        case "B" => "</B>"
        case "U" => "</U>"
        case "I" => "</I>"
      }
      c._1.toString.concat(close)

    } else {
      c._1.toString
    }
  }).mkString("")

  println(r)

}
