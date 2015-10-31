package stack

import scala.collection.mutable.ArrayBuffer
import linkedlist.SingleLinkedList

import scala.reflect.ClassTag

class LinkedListStack[T : ClassTag] {

  private val sll = new SingleLinkedList[T]

  def push(item: T) : Unit = {
    sll.insertFirst(item)
  }

  def pop() : T = {
    if(isEmpty()) throw new Exception("Stack Empty")
    sll.removeFirst()
  }

  def isEmpty() : Boolean = {
    sll.isEmpty()
  }

  def size() : Int = sll.size()

}

object LinkedListStackTest {

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
