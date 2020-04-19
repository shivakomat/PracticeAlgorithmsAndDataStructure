package common

object AverageWordLengths extends App {

  val string = "What is the difference between coronavirus and COVID-19"

  val listOfWords = string.split(" ").toSeq

  def lengthOfWord(aggregatorWordLength: Int, words: String): Int = {
    aggregatorWordLength + words.length
  }

  val totalSentenceLength =
    listOfWords.foldLeft(0)(lengthOfWord)


  println(totalSentenceLength / listOfWords.size)

}
