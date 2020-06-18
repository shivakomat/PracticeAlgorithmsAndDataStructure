package common

object ClimbingStairs {

  def climbStairs(n: Long): Long = {
    if (n == 1 || n == 0)
      return 1;
    else if (n == 2)
      return 2;

    else
      return climbStairs(n - 3) + climbStairs(n - 2) + climbStairs(n - 1);
  }

  println(climbStairs(3))

}
