package common

object MismatchingWordsBetweenStrings extends  App {

  val str1 = "characters"
  val str2 = "alphabets"


  var str1Map = str1.toCharArray.toSeq.distinct.map(c => c -> 1).toMap

  str2.toCharArray.toSeq.foreach(c => {
    if(str1Map.contains(c)) {
      str1Map += c -> -1
    } else {
      str1Map += c -> 2
    }
  })

  str1Map.filter(c => c._2 == 1 || c._2 == 2).keys.toSeq.distinct.foreach(print)

}
