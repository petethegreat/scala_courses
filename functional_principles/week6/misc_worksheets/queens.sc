import Math.abs
object queensObj {

    def isSafe(queensList:List[Int],col:Int): Boolean = {
      val row = queensList.length
      // we are adding a new horizontal row.
      // if col exists in the list, then we are on the same vertical
      // col - row is diagonal we are trying.
      //        !(queensList.contains(col) || queensList.zipWithIndex.map((c:Int,r:Int) => c - r ).contains(col - row))
      val existingPositions = (row-1 to 0 by -1).zip(queensList)
      existingPositions.forall({
        case (r,c) => (c != col) && abs(c-col) != abs(r - row) // (abs(col - row) != abs(c - r))
      })
    } // end isSafe

    def queens(n: Int): Set[List[Int]] = {
      println(s"invoking queens, n = $n")
      def placeQueen(k: Int): Set[List[Int]] =
        if (k == 0) Set(List())
        else
          for {
            queens <- placeQueen(k - 1)
            col <- 0 until n
            if isSafe(queens, col)
          } yield col::queens
      placeQueen(n)
    }    // end queens
//   println(queens(4))

  val bsize = 4
  println(bsize)
//  queens(bsize).foreach(l => {
//    println() ; l.foreach(r => println("_"*(r) + "Q" + "_"*(bsize-r -1)))}
//  )
//  println(queens(bsize))
  print(queens(bsize).map(x => x.foldLeft("\n")((z,y) => z + "_"*y  + "Q" + "_"*(bsize - y - 1) +"\n")))

  } // end queensObj

queensObj

