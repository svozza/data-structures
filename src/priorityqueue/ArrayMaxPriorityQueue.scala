package priorityqueue

import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

class ArrayMaxPriorityQueue[T <% Ordered[T] : ClassTag] {

  private var N = 0
  private var pq = new Array[T](1)

  def insert(item : T) : Unit = {
    if(isFull()) resize(pq.length * 2)
    pq(size() + 1) = item
    N += 1
    swim(size())
  }

  def removeMax() : T = {
    if(isEmpty()) throw new Exception("Priority Queue Empty")
    val item = max()
    pq(1) = pq(size())
    pq(size()) = null.asInstanceOf[T]
    N -= 1
    sink(1)
    if(N > 0 && N < pq.length / 4) resize(pq.length / 2)
    item
  }

  def max() : T = {
    if(isEmpty()) throw new Exception("Priority Queue Empty")
    pq(1)
  }

  def size() : Int = N

  def isEmpty() : Boolean = N == 0

  def toArray() : Array[T] = {
    var builder = ArrayBuilder.make[T]
    for(i <- pq.indices if i != 0 && i <= size()) {
        builder += pq(i)
    }
    builder.result()
  }

  private def resize(size: Int) : Unit = {
    val newArr = new Array[T](size)
    for(i <- pq.indices if i < size) {
      newArr(i) = pq(i)
    }
    pq = newArr
  }

  private def sink(k : Int) : Unit = {
    var done = false
    var parent = k
    while(2 * parent <= size() && !done) {
      var child = parent * 2
      if(child < size() && pq(child) < pq(child + 1)) child += 1
      if(pq(parent) > pq(child)) done = true
      else {
        swap(parent, child)
        parent = child
      }
    }
  }

  private def swim(k : Int) : Unit = {
    var parent = k
    while(parent > 1 && pq(parent) > pq(parent / 2)) {
      swap(parent, parent / 2)
      parent = parent / 2
    }
  }

  private def swap(i: Int, j : Int) : Unit = {
    val item = pq(i)
    pq(i) = pq(j)
    pq(j) = item
  }

  private def isFull() : Boolean = N == pq.length - 1

}

object ArrayMaxPriorityQueueTest {

  private def deepEquals(mpq: ArrayMaxPriorityQueue[Int], expected: Array[Int]) = {
    mpq.toArray().deep.equals(expected.deep)
  }

  def main (args: Array[String]) {

    val mpq = new ArrayMaxPriorityQueue[Int]()

    mpq.insert(1)
    mpq.insert(10)
    mpq.insert(6)
    mpq.insert(5)
    assert(mpq.max() == 10)
    mpq.insert(15)

    assert(deepEquals(mpq, Array[Int](15, 10, 6, 1, 5)))

    assert(mpq.removeMax() == 15)
    assert(deepEquals(mpq, Array[Int](10, 5, 6, 1)))

    mpq.insert(20)
    mpq.insert(2)
    mpq.insert(7)
    mpq.insert(10)
    assert(deepEquals(mpq, Array[Int](20, 10, 7, 10, 5, 2, 6, 1)))

    assert(mpq.removeMax() == 20)
    assert(deepEquals(mpq, Array[Int](10, 10, 7, 1, 5, 2, 6)))

    assert(mpq.removeMax() == 10)
    assert(deepEquals(mpq, Array[Int](10, 6, 7, 1, 5, 2)))
    assert(mpq.size() == 6)

    assert(mpq.removeMax() == 10)
    assert(mpq.removeMax() == 7)
    assert(mpq.removeMax() == 6)
    assert(mpq.removeMax() == 5)
    assert(mpq.removeMax() == 2)
    assert(mpq.removeMax() == 1)

    assert(mpq.size() == 0)

    println("Tests passed!");

  }

}


