package stack

import scala.reflect.ClassTag

class Stack[T: ClassTag] {

  private var arr = new Array[T](1)
  private var N = 0

  def push(item: T) : Unit = {
    if(isFull()) resize(arr.length * 2)
    arr(N) = item
    N +=1
  }

  def pop() : T = {
    if(isEmpty()) throw new Exception("Stack Empty")
    N -= 1
    if(N > 0 && N < arr.length / 4) resize(arr.length / 2)
    arr(N)
  }

  def isEmpty() : Boolean = {
    N == 0
  }

  def size() : Int = N

  private def isFull() : Boolean = {
    N == arr.length
  }

  private def resize(size: Int) : Unit = {
    val newArr = new Array[T](size)
    for(i <- arr.indices) {
      newArr(i) = arr(i)
    }
    arr = newArr
  }

}

object StackTest {

  def main(args: Array[String]) {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(6)
    stack.pop()
    stack.push(11)
    stack.push(5)
    stack.pop()
    stack.pop()
    stack.pop()
    println(stack.size())
  }

}