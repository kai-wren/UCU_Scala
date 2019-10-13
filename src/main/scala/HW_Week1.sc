import scala.annotation.tailrec

//Exercise 1

def exercise1(num1: Int, num2: Int, num3: Int): Double = {

  val res: Double = if (num1 >= num2 & num2 >= num3) scala.math.pow(num1, 2) + scala.math.pow(num2, 2)
  else if (num1 >= num2 & num2 < num3) scala.math.pow(num1, 2) + scala.math.pow(num3, 2)
  else if (num1 < num2 & num2 < num3) scala.math.pow(num2, 2) + scala.math.pow(num3, 2)
  else if (num1 < num2 & num2 >= num3 & num1 <= num3) scala.math.pow(num2, 2)+scala.math.pow(num3, 2)
  else 0
  res
}

//checking results
assert(exercise1(2, 3, 4) == 25)
assert(exercise1(5, 3, 4) == 41)
assert(exercise1(5, 4, 3) == 41)
assert(exercise1(2, 5, 4) == 41)
assert(exercise1(2, 4, 5) == 41)
assert(exercise1(3, 3, 3) == 18)
assert(exercise1(3, 3, 4) == 25)
assert(exercise1(4, 3, 3) == 25)
assert(exercise1(3, 4, 3) == 25)
assert(exercise1(3, 3, 2) == 18)
assert(exercise1(2, 3, 3) == 18)
assert(exercise1(3, 2, 3) == 18)

// Exercise 2

def goodEnough(num: Double, aprx_num: Double, diff: Double = 0.000000005): Boolean = {
  if (scala.math.abs(aprx_num - scala.math.sqrt(num)) <= diff) true
  else false
}

def sqrt(num: Double, xprev: Double = 1): Double = {
if (goodEnough(num, 0.5*(xprev+(num.toFloat/xprev)))) 0.5*(xprev+(num.toFloat/xprev))
else sqrt(num, 0.5*(xprev+(num.toFloat/xprev)))
}

//checking result.
sqrt(10)
scala.math.sqrt(10)

// Exercise 3
//define some functions to be passed as then- and else-clauses
def printTrue(): Unit = {
  println("true")
}

def printFalse(): Unit = {
  println("false")
}

def myIf(pred: =>Boolean, thenClause: =>Unit, elseClause:  =>Unit): Unit = {
pred match {
  case true => thenClause
  case false => elseClause
}
}

//checking result

myIf((1 < 2), thenClause = printTrue, elseClause = printFalse)
myIf((1 > 2), thenClause = printTrue, elseClause = printFalse)

//using myIf for exercise 2
def sqrtMyIf(num: Double, xprev: Double = 1): Unit = {
  myIf(goodEnough(num, 0.5*(xprev+(num.toFloat/xprev))), thenClause = print(0.5*(xprev+(num.toFloat/xprev))), elseClause = sqrtMyIf(num, 0.5*(xprev+(num.toFloat/xprev))))
}

//checking result
sqrtMyIf(10)
scala.math.sqrt(10)

//Exercise 4
//define a new check methods for cubic root
def goodEnoughCube(num: Double, aprx_num: Double, diff: Double = 0.00000005): Boolean = {
  if (scala.math.abs(aprx_num - scala.math.cbrt(num)) <= diff) true
  else false
}

def cubrt(num: Double, xprev: Double = 1): Double = {
  if (goodEnoughCube(num, (num.toFloat / scala.math.pow(xprev, 2) + 2*xprev).toFloat / 3)) (num.toFloat / scala.math.pow(xprev, 2) + 2*xprev).toFloat / 3
  else cubrt(num, (num.toFloat / scala.math.pow(xprev, 2) + 2*xprev).toFloat / 3)
}

//checking result
cubrt(10)
scala.math.cbrt(10)


// Exercise 5

def factorial(num: BigInt): BigInt = {
  if (num == 1 | num == 0) 1
  else num*factorial(num-1)
}


// checking results
assert(factorial(1) == 1)
assert(factorial(2) == 2)
assert(factorial(3) == 6)
assert(factorial(4) == 24)
assert(factorial(5) == 120)
assert(factorial(10) == 3628800)
assert(factorial(0) == 1)

// tail recursion for factorial
@tailrec
def factorial_tailrec(num: BigInt, res: BigInt = 1): BigInt = {
  if (num == 1 | num == 0) res
  else factorial_tailrec(num-1, res*num)
}

// checking results
assert(factorial_tailrec(1) == 1)
assert(factorial_tailrec(2) == 2)
assert(factorial_tailrec(3) == 6)
assert(factorial_tailrec(4) == 24)
assert(factorial_tailrec(5) == 120)
assert(factorial_tailrec(10) == 3628800)
assert(factorial_tailrec(0) == 1)

//Exercise 6
def fibonacci(num: BigInt): BigInt = {
  if (num == 0) 0
  else if(num == 1) 1
  else fibonacci(num-2) + fibonacci(num-1)
}

//checking result
assert(fibonacci(1) == 1)
assert(fibonacci(0) == 0)
assert(fibonacci(2) == 1)
assert(fibonacci(3) == 2)
assert(fibonacci(4) == 3)
assert(fibonacci(5) == 5)
assert(fibonacci(6) == 8)
assert(fibonacci(7) == 13)
assert(fibonacci(8) == 21)
assert(fibonacci(9) == 34)
assert(fibonacci(10) == 55)

// tail recursion fibonacci
@tailrec
def fibonacci_tailrec(num: BigInt = 0, num_start: BigInt = 0, res: BigInt = 0, res_prev: BigInt = 0): BigInt = {
  if (num_start == 0) fibonacci_tailrec(num = num, num_start = num_start + 1)
  else if (num_start == 1) fibonacci_tailrec(num = num, num_start = num_start + 1, res = 1 + res, res_prev = res)
  else if (num_start == num) res + res_prev
  else if (num == 1) 1
  else if (num == 0) 0
  else fibonacci_tailrec(num = num, num_start = num_start + 1, res = res + res_prev, res_prev = res)
}

//checking result
assert(fibonacci_tailrec(1) == 1)
assert(fibonacci_tailrec(0) == 0)
assert(fibonacci_tailrec(2) == 1)
assert(fibonacci_tailrec(3) == 2)
assert(fibonacci_tailrec(4) == 3)
assert(fibonacci_tailrec(5) == 5)
assert(fibonacci_tailrec(6) == 8)
assert(fibonacci_tailrec(7) == 13)
assert(fibonacci_tailrec(8) == 21)
assert(fibonacci_tailrec(9) == 34)
assert(fibonacci_tailrec(10) == 55)


//Exercise 7

def nck(n: Int, k: Int): BigInt = {
  factorial_tailrec(n) / (factorial_tailrec(k) * factorial_tailrec(n-k))
}

def pascal_triangle(max_depth: Int, curr_depth: Int = 0): Unit = {
for (i <- 0 to curr_depth) {
  printf("%d ", nck(curr_depth, i))
}
  println()
  if (curr_depth < max_depth) pascal_triangle(max_depth=max_depth, curr_depth = curr_depth + 1)
}

// checking result. output starts from row 0, hence depth 14 means 15 rows
//will be in output
pascal_triangle(14)

// Exercise 8

def isPrime(num: BigInt, div: BigInt = 2): Boolean = {
if (num == 2) true
  else if (num < 2) false
  else if (num % div == 0) false
  else if (scala.math.pow(div.toDouble, 2).toInt > num) true
  else isPrime(num, div+1)
}

//checking result
val checkNum = 123123323
if (isPrime(checkNum)) printf("Number %d is prime!", checkNum)
else printf("Number %d is not prime!", checkNum)

// Exercise 9
// method used to get n-th element of given evaluation list. Output is element value itself.
def getNthElement(idx: Int, l: List[String]): String = {
  (idx, l) match {
    case (0, head :: _) => head
    case (idx, _ :: tail) => getNthElement(idx-1, tail)
    case (_, Nil) => Nil.toString
  }
}

//method to find first occurrence of given element in the list. Output: sub-list before element,
// index of a element and sublist after element.
def findElement(elem: String, l: List[String], idx: Int = 0, headAcc: List[String] = List()): Any = {
  l match {
    case (head :: tail) => if (head == elem) (headAcc, idx, tail) else findElement(elem, tail, idx+1, headAcc :+ head)
    case Nil => (Nil, 0, Nil)
  }
}

//method used to remove last element of given list without using List API
def removeLastElement(l: List[String], idx: BigInt = 0, newList: List[String] = Nil, prevHead: String = Nil.toString): List[String] = {
  (l, idx, prevHead) match {
    case (h :: tail, idx, pH) => {
      if (pH == Nil.toString) removeLastElement(tail, idx+1, newList, h)
      else removeLastElement(tail, idx+1, newList :+ prevHead, h)
    }
    case (Nil, _, _) => newList
  }
}

//methos used to parse trough the list using methods above and replace all product operations of any two numbers
// by product result. Process runs till all product operations are calculated.
// Output - new list without product operations
def multiply(l: List[String]): List[String] = {
  val (headList: List[String], multIdx: Int, tailList: List[String]) = findElement("*", l)
  if (multIdx > 0){
    val _::t = tailList
    multiply(removeLastElement(headList) ::: (getNthElement(multIdx-1, l).toInt * getNthElement((multIdx.toInt + 1), l).toInt).toString :: t)
  }
  else l
}

// this method used to calculate all addition operations which the only ones left after multiplication performed.
// Output is final number calculated.
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

//Just call multiplication and addition methods in a row - one after another one.
def eval(l: List[String]): BigInt = {
  add(multiply(l))
}

assert(eval(List("1", "+", "2", "*", "3")) == 7)
assert(eval(List("5", "*", "3", "+", "1")) == 16)
assert(eval(List("1", "+", "2", "*", "3", "*", "4", "+", "1", "*", "2")) == 27)
assert(eval(List("1", "*", "2", "+", "3", "*", "4", "*", "2", "+", "3", "*", "3")) == 35)
