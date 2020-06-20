package capitalOneInterview

import scala.collection.mutable.Queue

object CapitalOne extends App {

  val items1 = List("alpha", "beta", "gamma", "gamma")
  val items2 = List("beta", "beta", "beta")
  val items3 = List("alpha", "alpha", "alpha", "beta", "beta", "beta", "gamma")

  val windowSize = 6
  val words : Queue[String] = Queue()

def add(items: List[String]): Unit = {
    items.foreach( item => {
      if(words.size > windowSize) {
        words.dequeue()
        words.enqueue(item)
      } else {
        words.enqueue(item)
      }
    })
}

def printCounts(): Unit = {
  println(
    words
      .groupBy(e => e)
      .mapValues(_.size).toSeq
      .sortBy(_._2).reverse
      .map(w => (w._2 + " " + w._1))
      .mkString(", "))
}

  add(items1)
  printCounts()
  add(items2)
  printCounts()
  add(items3)
  printCounts()

}