package linkedlist

import scala.collection.mutable.ArrayBuffer

class SingleLinkedList[T] {

  private var first:Node = null
  private var last: Node = null
  private var N = 0

  private class Node(val item: T, var next: Node) {
  }

  def insertFirst(item: T) : Unit = {
      val oldFirst = first
      first = new Node(item, oldFirst)
      if(isEmpty()) {
        last = first
      }
      N += 1
  }

  def insertLast(item: T) : Unit = {
    if(isEmpty()) {
      first = new Node(item, last)
      last = first
    }
    else {
      val oldLast = last
      last = new Node(item, null)
      oldLast.next = last
    }
    N += 1
  }

  def removeFirst() : T = {
    if(isEmpty()) throw new Exception("List is empty.")
    val item = first.item
    first = first.next
    N -= 1
    item
  }

  def removeLast() : T = {
    var item = null.asInstanceOf[T]
    if(isEmpty()) throw new Exception("List is empty.")

    if(size() == 1) {
      item = first.item
      first = null
      last = null
      return item
    }

    var x = first
    while(x != null) {
      if(x.next == last) {
        item = x.next.item
        x.next = null
        last = x
      }
      x = x.next
    }
    return item
  }

  def size() : Int = N

  def isEmpty() : Boolean = N == 0

}

object SingleLinkedListTest {

  def main(args: Array[String]) {
    val actual = new ArrayBuffer[Int]()
    val expected = Array[Int](20, 12, 10, 9)
    val sll = new SingleLinkedList[Int]
    sll.insertFirst(3)
    sll.insertFirst(10)
    sll.insertLast(20)
    sll.insertFirst(12)
    actual += sll.removeLast()
    sll.insertLast(7)
    sll.insertLast(9)
    actual += sll.removeFirst()
    actual += sll.removeFirst()
    actual += sll.removeLast()

    for(i <- actual.indices) {
      assert(actual(i) == expected(i))
    }

    println("Test Passed!")
  }
}
