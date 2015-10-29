package stack

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class ArrayStack[T: ClassTag] {

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
    val item = arr(N)
    arr(N) = null.asInstanceOf[T]
    if(N > 0 && N < arr.length / 4) resize(arr.length / 2)
    item
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

object ArrayStackTest {

  def main(args: Array[String]) {
    val actual = new ArrayBuffer[Int]()
    val expected = Array[Int](6, 5, 11, 1)
    val stack = new ArrayStack[Int]
    stack.push(1)
    stack.push(6)
    actual += stack.pop()
    stack.push(11)
    stack.push(5)
    actual += stack.pop()
    actual += stack.pop()
    actual += stack.pop()

    for(i <- actual.indices) {
      assert(actual(i) == expected(i))
    }

    println("Test Passed!")
  }

}