package linkedlist

import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

class CircularLinkedList[T : ClassTag] {

  private var last:Node = null
  private var N = 0

  private class Node(val item: T, var next: Node) {
  }

  def insertFirst(item: T) : Unit = {
    if(isEmpty()) {
      last = new Node(item, null)
      last.next = last
    }
    else {
      last.next = new Node(item, last.next)
    }
    N += 1
  }

  def insertLast(item: T) : Unit = {
    if(isEmpty()) {
      last = new Node(item, null)
      last.next = last
    }
    else {
      val newLast = new Node(item, last.next)
      last.next = newLast
      last = newLast
    }
    N += 1
  }

  def removeFirst() : T = {
    if(isEmpty()) throw new Exception("List is empty.")
    val item = last.next.item
    if(size() == 1) last = null
    else last.next = last.next.next
    N -= 1
    item
  }

  def size() : Int = N

  def isEmpty() : Boolean = N == 0

  def toArray() : Array[T] = {
    var builder = ArrayBuilder.make[T]
    traverse(node => {
      builder += node.item
    })
    builder.result()
  }

  private def traverse(f: (Node) => Unit) : Unit = {
    if(!isEmpty()) {
      var x = last.next
      while(x != last) {
        f(x)
        x = x.next
      }
      f(last)
    }
  }

}

object CircularLinkedListTest {

  private def deepEquals(cll: CircularLinkedList[Int], expected: Array[Int]) = {
    cll.toArray().deep.equals(expected.deep)
  }

  def main(args: Array[String]) {
    val cll = new CircularLinkedList[Int]
    cll.insertFirst(3)
    cll.insertFirst(10)
    cll.insertFirst(12)
    cll.insertLast(20)
    cll.insertLast(40)
    assert(deepEquals(cll, Array[Int](12, 10, 3, 20, 40)))

    cll.removeFirst()
    cll.removeFirst()
    cll.removeFirst()
    cll.removeFirst()
    cll.removeFirst()
    assert(deepEquals(cll, Array[Int]()))

    cll.insertFirst(5)
    cll.insertLast(7)
    cll.insertFirst(4)
    assert(deepEquals(cll, Array[Int](4, 5, 7)))

    println("Test Passed!")
  }
}
