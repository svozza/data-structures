package linkedlist

import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

class SingleLinkedList[T : ClassTag] {

  private var first:Node = null
  private var last: Node = null
  private var N = 0

  private class Node(val item: T, var next: Node) {
  }

  def insertFirst(item: T) : Unit = {
      val oldFirst = first
      first = new Node(item, oldFirst)
      if(isEmpty()) last = first
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

  def insertAfter(after: T, item: T) : Unit = {
    if(isEmpty()) throw new Exception("List is empty.")
    traverse(node => {
      if(node.item == after) {
        val insert = new Node(item, node.next)
        node.next = insert
        if(node == last) last = insert
      }
    })
    N += 1
  }

  def removeAfter(after: T) : T = {
    if(isEmpty()) throw new Exception("List is empty.")
    var item = null.asInstanceOf[T]
    traverse(node => {
      if(node.item == after) {
        if(node == last) throw new Exception("Cannot call removeAfter on last element of the list.")
        item = node.next.item
        node.next = node.next.next
        if(node.next == null) last = node
      }
    })
    N -= 1
    if(size() == 1) last = first
    item
  }

  def removeFirst() : T = {
    if(isEmpty()) throw new Exception("List is empty.")
    val item = first.item
    if(size() == 1) {
      first = null
      last = null
    }
    else {
      first = first.next
    }
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
    }
    else {
      traverse(node => {
        if(node.next == last) {
          item = node.next.item
          node.next = null
          last = node
        }
      })
    }
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
    var x = first
    while(x != null) {
      f(x)
      x = x.next
    }
  }

}

object SingleLinkedListTest {

  private def deepEquals(sll: SingleLinkedList[Int], expected: Array[Int]) = {
    sll.toArray().deep.equals(expected.deep)
  }

  def main(args: Array[String]) {
    val sll = new SingleLinkedList[Int]
    sll.insertFirst(3)
    sll.insertFirst(10)
    sll.insertLast(20)
    sll.insertFirst(12)
    assert(deepEquals(sll, Array[Int](12, 10, 3, 20)))

    sll.removeLast()
    sll.insertLast(7)
    sll.insertLast(9)
    assert(deepEquals(sll, Array[Int](12, 10, 3, 7, 9)))

    sll.removeFirst()
    sll.removeFirst()
    sll.removeLast()
    assert(deepEquals(sll, Array[Int](3, 7)))

    sll.insertAfter(7, 50)
    sll.insertAfter(3, 12)
    sll.removeAfter(12)
    assert(deepEquals(sll, Array[Int](3, 12, 50)))

    sll.removeAfter(12)
    sll.removeAfter(3)
    assert(deepEquals(sll, Array[Int](3)))

    sll.removeLast()
    sll.insertLast(25)
    sll.insertAfter(25, 1)
    sll.insertAfter(1, 17)
    assert(deepEquals(sll, Array[Int](25, 1, 17)))

    println("Test Passed!")
  }
}
