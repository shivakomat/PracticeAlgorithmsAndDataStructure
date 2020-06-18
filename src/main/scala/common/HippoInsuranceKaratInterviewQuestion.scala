package common

object HippoInsuranceKaratInterviewQuestion extends App {
    val parentChildPairs1 = List((1, 3), (2, 3), (3, 6), (5, 6), (5, 7), (4, 5), (4, 8), (4, 9), (9, 11), (14, 4), (13, 12), (12, 9))
    val parentChildPairs2 = List((11, 10), (11, 12), (2, 3), (10, 2), (10, 5), (1, 3), (3, 4), (5, 6), (5, 7), (7, 8))

    def findNodesWithZeroAndOneParents(pairs: Seq[(Int, Int)]): (Seq[Int], Seq[Int]) = {
          val nodesWithParents = pairs.groupBy(_._2).keys.toSeq
          val allParents = pairs.map(_._1).distinct
          val nodesWithZeroParents = allParents.filterNot(nodesWithParents.contains(_))
          (nodesWithParents, nodesWithZeroParents)
    }

    def hasCommonAncestor(pairs: Seq[(Int, Int)], child1: Int, child2: Int): Boolean = {
      val nodesWithParentsInfo = pairs.groupBy(_._2).mapValues(v => v.map(_._1))

      def getAllAncestors(childNode: Int,
                          parents: Seq[Int] = nodesWithParentsInfo.getOrElse(child1, Seq.empty[Int]),
                          result: Seq[Int] = Seq.empty[Int]): Seq[Int] = {
        if(parents.isEmpty) result // return result
        else
          getAllAncestors(childNode, parents = parents.flatMap(n => nodesWithParentsInfo.getOrElse(n, Seq.empty[Int])), result = result ++ parents)
      }

      val child1Ancestors = getAllAncestors(child1)
      val child2Ancestors = getAllAncestors(child2).toSet

      (child1Ancestors.count(child2Ancestors.contains) > 0)
    }

    println(hasCommonAncestor(parentChildPairs2, 4, 12))
    println(hasCommonAncestor(parentChildPairs2, 1, 6))

}


/*
Question - 1

Suppose we have some input data describing a graph of relationships between parents and children over multiple generations. The data is formatted as a list of (parent, child) pairs, where each individual is assigned a unique integer identifier.

For example, in this diagram, 3 is a child of 1 and 2, and 5 is a child of 4:

1   2    4
 \ /   / | \
  3   5  8  9
   \ / \     \
    6   7    11

Sample input/output (pseudodata):

parentChildPairs = [
    (1, 3), (2, 3), (3, 6), (5, 6),
    (5, 7), (4, 5), (4, 8), (4, 9), (9, 11)
]

Write a function that takes this data as input and returns two collections: one containing all individuals with zero known parents, and one containing all individuals with exactly one known parent.

Output may be in any order:

findNodesWithZeroAndOneParents(parentChildPairs) => [
  [1, 2, 4],       // Individuals with zero parents
  [5, 7, 8, 9, 11] // Individuals with exactly one parent
]

n: number of pairs in the input

*/



/*

Question - 2

Suppose we have some input data describing a graph of relationships between parents and children over multiple generations. The data is formatted as a list of (parent, child) pairs, where each individual is assigned a unique integer identifier.

For example, in this diagram, 6 and 8 have common ancestors of 4 and 14.

         14  13
         |   |
1   2    4   12
 \ /   / | \ /
  3   5  8  9
   \ / \     \
    6   7     11

parentChildPairs1 = [
    (1, 3), (2, 3), (3, 6), (5, 6), (5, 7), (4, 5),
    (4, 8), (4, 9), (9, 11), (14, 4), (13, 12), (12, 9)
]

3 -> 1,2
8 -> 4
4 -> 14
1 ->
2 ->

(5, 8)

5 -> 4
8 -> 4
4 -> 14

(7, 11)
7 -> 5
11 -> 9
5 -> 4
9 -> 4

(7 -> 5, 4)
(11 -> 5, 4)
if(diff > 0) true else false

Write a function that takes the graph, as well as two of the individuals in our dataset, as its inputs and returns true if and only if they share at least one ancestor.

Sample input and output:

hasCommonAncestor(parentChildPairs1, 3, 8) => false
hasCommonAncestor(parentChildPairs1, 5, 8) => true
hasCommonAncestor(parentChildPairs1, 6, 8) => true
hasCommonAncestor(parentChildPairs1, 6, 9) => true
hasCommonAncestor(parentChildPairs1, 1, 3) => false
hasCommonAncestor(parentChildPairs1, 3, 1) => false
hasCommonAncestor(parentChildPairs1, 7, 11) => true
hasCommonAncestor(parentChildPairs1, 6, 5) => true
hasCommonAncestor(parentChildPairs1, 5, 6) => true

Additional example: In this diagram, 4 and 12 have a common ancestor of 11.

        11
       /  \
      10   12
     /  \
1   2    5
 \ /    / \
  3    6   7
   \        \
    4        8

parentChildPairs2 = [
    (11, 10), (11, 12), (2, 3), (10, 2), (10, 5),
    (1, 3), (3, 4), (5, 6), (5, 7), (7, 8),
]

hasCommonAncestor(parentChildPairs2, 4, 12) => true
hasCommonAncestor(parentChildPairs2, 1, 6) => false
hasCommonAncestor(parentChildPairs2, 1, 12) => false

n: number of pairs in the input

*/