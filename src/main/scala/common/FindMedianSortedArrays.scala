package common

object FindMedianSortedArrays extends  App {


    def findMedianSortedArrays(nums1: Seq[Int], nums2: Seq[Int]): Double = {

      def combineArraysInSorted(nums1: Seq[Int], nums2: Seq[Int]): Seq[Int] =
        (nums1 ++ nums2).sorted

      def findMedian(nums: Seq[Int]): Double = {
        val numberOfElements = nums.size
        val middleModulus = numberOfElements % 2
        middleModulus match {
          case 1 => nums(numberOfElements / 2).toDouble
          case 0 => {
            val middle = numberOfElements / 2
            (nums(middle) + nums(middle - 1)) / 2.0
          }
        }
      }

      val finalList = combineArraysInSorted(nums1.toSeq, nums2.toSeq)
      findMedian(finalList)

   }


  val result = findMedianSortedArrays(Seq(1, 3), Seq(2, 4))
  assert(result == 2.5)
}
