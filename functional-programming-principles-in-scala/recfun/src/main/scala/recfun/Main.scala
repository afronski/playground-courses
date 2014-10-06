package recfun

import common._

object Main {
  
  def main(args: Array[String]) {
    println("Pascal's Triangle")

    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r)
      throw new IllegalArgumentException()
    else if (c == 0 || c == r) 1
    else if (r < 2) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  def balance(chars: List[Char]): Boolean = {
    
    def checkChar(char: Char, balance: Int): Int =
      if (char == '(') balance + 1
      else if (char == ')' && balance > 0) balance - 1
      else if (char == ')' && balance <= 0) balance - 2
      else balance
      
    def balanceIter(chars: List[Char], balance: Int): Int = 
       if (chars.isEmpty) balance
       else balanceIter(chars.tail, checkChar(chars.head, balance))
       
    balanceIter(chars, 0) == 0
  }

  def countChange(money: Int, coins: List[Int]): Int = {    
    if (coins.isEmpty) 0
    else if (money < 0 ) 0
    else if (money == 0) 1
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
