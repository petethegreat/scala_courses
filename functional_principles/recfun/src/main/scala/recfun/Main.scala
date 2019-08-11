package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c > r) throw new IllegalArgumentException("c must be <= r")
      if (c == r  |  c == 0) {1}
      else {pascal(c-1, r-1) + pascal(c, r-1)}
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
    {
      // define an inner function for recursion through the sequence
      @scala.annotation.tailrec
      def open_close(a: List[Char], opened: Int): Int = {
        // opened counts the number of opened parentheses
        // a contains the (remainder of ) the sequence of Char to be checked
        //  Bail out if opened is ever negative (we close before we open)
        if (a.isEmpty) {
          opened
        }
        // if there's nothing left to check, we're done
        else {
          if (a.head == '(') {
            open_close(a.tail, opened + 1)
          }
          // if the character is an opening, then increment opened
          else if (a.head == ')') {
            if (opened <1) {-1}
            else{open_close(a.tail, opened - 1)}
            // if it's a closing, decrement
          }
          else {
            open_close(a.tail, opened)
          }
          // otherwise pass opened through
        }
      }
      ///////////// open_close
      // do the test
      open_close(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
