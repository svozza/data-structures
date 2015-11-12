package queue

import linkedlist.CircularLinkedList

import scala.reflect.ClassTag

class CircularLinkedListQueue[T : ClassTag] {

  private val cll = new CircularLinkedList[T]

  def enqueue(item : T) = cll.insertLast(item)

  def dequeue() : T = cll.removeFirst()

  def size() = cll.size()

  def isEmpty() = cll.isEmpty()

  def toArray() = cll.toArray()

}

object CircularLinkedListQueueTest {

  val queue = new CircularLinkedListQueue[Int]

  private def deepEquals(cll: CircularLinkedListQueue[Int], expected: Array[Int]) = {
    queue.toArray().deep.equals(expected.deep)
  }

  def main (args: Array[String]){
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    queue.enqueue(4)
    assert(deepEquals(queue, Array[Int](1, 2, 3, 4)))

    queue.dequeue()
    assert(deepEquals(queue, Array[Int](2, 3, 4)))
    queue.dequeue()
    assert(deepEquals(queue, Array[Int](3, 4)))
    queue.dequeue()
    assert(deepEquals(queue, Array[Int](4)))
    queue.dequeue()
    assert(deepEquals(queue, Array[Int]()))

    println("Test Passed!")
  }

}