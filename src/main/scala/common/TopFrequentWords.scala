package common

object TopFrequentWords extends App {

    def topKFrequent(words: Array[String], k: Int): List[String] = {
      case class Word(wrd: String, count: Int = 0)

      var hashMap = Map.empty[String, (Int, Int)]

      words.toSeq.zipWithIndex.foreach(wordWithPostion => {
        val wrd = wordWithPostion._1
        val position = wordWithPostion._2
        if(hashMap.get(wrd).isEmpty) {
          hashMap += wrd -> (1, position)
        } else {
          val wordWithCounts = hashMap.get(wrd).get
          val  newCount = wordWithCounts._1 + 1
          hashMap += wrd -> (newCount, wordWithCounts._2)
        }
      })

      val frequencyOfTopKWords = hashMap.toSeq.sortBy(_._2).reverse.take(k)
      frequencyOfTopKWords.sortBy(_._2._2).map(_._1).toList
    }


  val words = Array("the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is")
  val topK = 4

  topKFrequent(words, topK).foreach(w => print(w + ", "))
}