package common

object LargestSumSubArray extends App {

  val numbers = Seq(-2,1,-3,4,-1,2,1,-5,4);

  // answer = 6

  def largestSum(numbers: Seq[Int]): Int = {
    var currentIndex = 0
    var largestSum= 0
    var currentSum = 0
    var startSubArrayIndex = 0
    var endSubArrayIndex = 0
    var s = 0

    for(number <- numbers) {
      currentSum = number + currentSum

      if(largestSum < currentSum) {
        largestSum = currentSum
        startSubArrayIndex = s
        endSubArrayIndex = currentIndex
      }

      if(currentSum < 0) {
        currentSum = 0
        s = currentIndex + 1
      }


    }

    largestSum

  }


  println(largestSum(numbers))
}
