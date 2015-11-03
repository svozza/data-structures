package linkedlist

import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

class DoubleLinkedList[T: ClassTag] {

  private var first:Node = null
  private var last: Node = null
  private var N = 0

  private class Node(val item: T, var prev:Node, var next: Node) {
  }

  def insertFirst(item: T) : Unit = {
    if(isEmpty()) {
      first = new Node(item, null, null)
      last = first
    }
    else {
      val oldFirst = first
      first = new Node(item, null, oldFirst)
      oldFirst.prev = first
    }
    N += 1
  }

  def insertBefore(before: T, item: T) : Unit = {
    if(isEmpty()) throw new Exception("List is empty.")
    traverse(node => {
      if(node.item == before) {
        if (node == first) insertFirst(item)
        else {
          val insert = new Node(item, node.prev, node)
          node.prev.next = insert
          node.prev = insert
        }
      }
    })
    N += 1
  }

  def insertLast(item: T) : Unit = {
    if(isEmpty()) {
      first = new Node(item, null, null)
      last = first
    }
    else {
      val oldLast = last
      last = new Node(item, oldLast, null)
      oldLast.next = last
    }
    N += 1
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
      first.prev = null
    }
    N -= 1
    item
  }

  def removeBefore(before: T) : T = {
    if(isEmpty()) throw new Exception("List is empty.")
    if(first.item == before) throw new Exception("Cannot call removeBefore on first element of the list.")
    if(first.next.item == before) return removeFirst()
    var item = null.asInstanceOf[T]
    traverse(node => {
      if(node.item == before) {
          item = node.prev.item
          node.prev = node.prev.prev
          node.prev.next = node
      }
    })
    N -= 1
    if(size() == 1) first = last
    item
  }

  def removeLast() : T = {
    if(isEmpty()) throw new Exception("List is empty.")
    val item = last.item

    if(size() == 1) {
      first = null
      last = null
    }
    else {
      last = last.prev
      last.next = null
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

object DoubleLinkedListTest {

  val dll = new DoubleLinkedList[Int]

  private def deepEquals(dll: DoubleLinkedList[Int], expected: Array[Int]) = {
    dll.toArray().deep.equals(expected.deep)
  }

  def main (args: Array[String]){
    dll.insertFirst(3)
    dll.insertFirst(2)
    dll.insertFirst(1)
    assert(deepEquals(dll, Array[Int](1, 2, 3)))

    dll.insertLast(4)
    dll.insertLast(5)
    assert(deepEquals(dll, Array[Int](1, 2, 3, 4, 5)))

    assert(dll.removeFirst() == 1)
    assert(deepEquals(dll, Array[Int](2, 3, 4, 5)))

    assert(dll.removeFirst() == 2)
    assert(deepEquals(dll, Array[Int](3, 4, 5)))

    assert(dll.removeLast() == 5)
    assert(deepEquals(dll, Array[Int](3, 4)))

    assert(dll.removeLast() == 4)
    assert(deepEquals(dll, Array[Int](3)))

    assert(dll.removeLast() == 3)
    assert(deepEquals(dll, Array[Int]()))

    dll.insertLast(5)
    dll.insertFirst(3)
    dll.insertBefore(5, 4)
    dll.insertFirst(1)
    dll.insertBefore(3, 2)
    assert(deepEquals(dll, Array[Int](1, 2, 3, 4, 5)))

    dll.removeBefore(5)
    dll.removeBefore(2)
    dll.removeBefore(3)
    assert(deepEquals(dll, Array[Int](3, 5)))

    println("Test Passed!")
  }

}
