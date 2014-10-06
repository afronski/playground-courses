package funsets

object Main extends App {
  import FunSets._
  
  printSet(map((x : Int) => x == 0 || x == 2 || x == 4 || x == 1000, (x : Int) => x + 1))
  printSet(map((x : Int) => x == 0 || x == 2 || x == 4 || x == 1000, (x : Int) => x - 1))
  printSet(map((x : Int) => x == 1 || x == 3 || x == 5 || x == 7, (x : Int) => x * 2))
}

