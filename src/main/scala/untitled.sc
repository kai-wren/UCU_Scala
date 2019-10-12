def getNthElement(idx: Int, l: List[String]): String = {
  (idx, l) match {
    case (0, head :: _) => head
    case (idx, _ :: tail) => getNthElement(idx-1, tail)
    case (_, Nil) => Nil.toString
  }
}

//getNthElement(0, List("1", "+", "2", "*", "3"))

def findElement(elem: String, l: List[String], idx: Int = 0, headAcc: List[String] = List()): Any = {
  l match {
    case (head :: tail) => if (head == elem) (headAcc, idx, tail) else findElement(elem, tail, idx+1, headAcc :+ head)
    case Nil => (Nil, 0, Nil)
  }
}

//findElement("*", List("1", "+", "2", "*", "3", "*", "4", "+", "1", "*", "2"))

def removeLastElement(l: List[String], idx: BigInt = 0, newList: List[String] = Nil, prevHead: String = Nil.toString): List[String] = {
  (l, idx, prevHead) match {
    case (h :: tail, idx, pH) => {
      if (pH == Nil.toString) removeLastElement(tail, idx+1, newList, h)
      else removeLastElement(tail, idx+1, newList :+ prevHead, h)
    }
    case (Nil, _, _) => newList
  }
}

//removeLastElement(List("1", "+", "2"))

def multiply(l: List[String]): List[String] = {
  val (headList: List[String], multIdx: Int, tailList: List[String]) = findElement("*", l)
  if (multIdx > 0){
    val _::t = tailList
    multiply(removeLastElement(headList) ::: (getNthElement(multIdx-1, l).toInt * getNthElement((multIdx.toInt + 1), l).toInt).toString :: t)
  }
  else l
}

//multiply(List("1", "+", "2", "*", "3", "*", "4", "+", "1", "*", "2"))
//multiply(List("1", "*", "2", "+", "3", "*", "4", "*", "2", "+", "3", "*", "3"))

def add(l: List[String], res: BigInt = 0): BigInt = {
  val (headList: List[String], addIdx: Int, tailList: List[String]) = findElement("+", l)
  if (addIdx > 0)  {
    val _::t = tailList
    if (getNthElement(addIdx+2, (getNthElement(addIdx-1, l).toInt + getNthElement(addIdx+1, l).toInt).toString :: t) != Nil.toString){
      add((getNthElement(addIdx-1, l).toInt + getNthElement(addIdx+1, l).toInt).toString :: t,
        res + getNthElement(addIdx-1, l).toInt + getNthElement(addIdx+1, l).toInt)
    }
    else add((getNthElement(addIdx-1, l).toInt + getNthElement(addIdx+1, l).toInt).toString :: t,
      getNthElement(addIdx-1, l).toInt + getNthElement(addIdx+1, l).toInt)
  }
  else res
}

//add(List("1", "+", "24", "+", "2"))
//add(List("2", "+", "24", "+", "9"))


def eval(l: List[String]): BigInt = {
  add(multiply(l))
}

assert(eval(List("1", "+", "2", "*", "3")) == 7)
assert(eval(List("5", "*", "3", "+", "1")) == 16)
assert(eval(List("1", "+", "2", "*", "3", "*", "4", "+", "1", "*", "2")) == 27)
assert(eval(List("1", "*", "2", "+", "3", "*", "4", "*", "2", "+", "3", "*", "3")) == 35)