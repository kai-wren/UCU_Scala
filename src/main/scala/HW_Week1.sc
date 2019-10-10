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

def goodEnough(num: Double, aprx_num: Double, diff: Double = 0.00005): Boolean = {
  if (scala.math.abs(aprx_num - scala.math.sqrt(num)) <= diff) true
  else false
}

def sqrt(num: Double, xprev: Double = 1): Double = {
if (goodEnough(num, 0.5*(xprev+(num.toFloat/xprev)))) 0.5*(xprev+(num.toFloat/xprev))
else sqrt(num, 0.5*(xprev+(num.toFloat/xprev)))
}

//checking result
sqrt(10)
scala.math.sqrt(10)

// Exercise 3

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

def goodEnoughCube(num: Double, aprx_num: Double, diff: Double = 0.00005): Boolean = {
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

// tail recursion
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

// tail recursion
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

def pascal_triangle(max_deepth: Int, curr_deepth: Int = 0): Unit = {
for (i <- 0 to curr_deepth) {
  printf("%d ", nck(curr_deepth, i))
}
  println()
  if (curr_deepth < max_deepth) pascal_triangle(max_deepth=max_deepth, curr_deepth = curr_deepth + 1)
}

// checking result. output tarts from row 0, hence deepth 14 means 15 rows
//will be in output
pascal_triangle(14)


