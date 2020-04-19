package common

object FindPrimeNumbers extends App {
  val n = 7
  def isPrime(n: Int): Boolean = {
    !(n / 2.0).isWhole()
  }
  (1 to n).flatMap(n => Some(n).filter(isPrime)).foreach(println)
}
