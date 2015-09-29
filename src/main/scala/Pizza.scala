package pizza

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue
import scalaz.syntax.either._
/**
 * Created by admin on 6/4/15.
 */

object Pizza {

  private def getAvgWaitTime(orders: Array[(Int, Int)]) = {

    @tailrec
    def sumServeTime(remaining: List[(Int, Int)], maxPlanningTime:Int, sum:Int, minHeap: PriorityQueue[(Int, Int)]):Int = {
      if (minHeap.isEmpty) {
        if (remaining.isEmpty) sum
        else sumServeTime(remaining.tail, remaining.head._1, sum, minHeap += remaining.head)
      } else {
        val (orderTime, cookDuration) = minHeap.dequeue()
        val newMaxPlanningTime = maxPlanningTime + cookDuration
        val (upcomingOrders, tail) = remaining.span{case(nextOrderTime , _) => newMaxPlanningTime > nextOrderTime}
        sumServeTime(tail, newMaxPlanningTime, sum + newMaxPlanningTime - orderTime, minHeap ++= upcomingOrders)
      }
    }

    val intervalOrdering = Ordering.by[(Int, Int), Int]{ case(orderTime, cookDuration) => orderTime - cookDuration}

    sumServeTime(orders.toList.sortBy(_._1), 0,0, PriorityQueue()(intervalOrdering)) / orders.length

  }

  private def getValidatedOrders(a: Array[String]) = {

    val num = "-?\\d+"
    val (timesMax, nMax) = (Math.pow(10, 9), Math.pow(10, 5))

    lazy val intHead = a.head.toInt
    lazy val tail = a.tail.map(_.split("\\s+"))
    lazy val pairs = tail.map(_.map(_.toInt)).map(a=> a(0) -> a(1))
    def notPairs = tail.filterNot(_.size == 2).map(_.mkString(" ")).toList
    def notNumbers = tail.filterNot(_.forall(_.matches(num))).map(_.mkString(" ")).toList
    def notInBounds = pairs.filter{ case (orderTime, cookDuration) =>
      orderTime < 0 || cookDuration < 1 || orderTime > timesMax  || cookDuration > timesMax
    }.toList


    if (a.isEmpty) "please provide input data".left
    else if (!a.head.matches(num) || intHead < 0 || intHead > nMax) s"first element should be a number in range 0 - $nMax".left
    else if(tail.length != intHead) "first element is not equal the length of the following elements".left
    else if(notPairs.nonEmpty) s"the following elements have corrupted fomat: ${notPairs}".left
    else if(notNumbers.nonEmpty)s"the following elements contain non number values: ${notNumbers}".left
    else if(notInBounds.nonEmpty) s"the following elements are not in bounds: ${notInBounds}".left
    else pairs.right
  }

  def apply(args:Array[String])= getValidatedOrders(args).map(getAvgWaitTime)

  def main(args: Array[String]) = apply(args).fold(println(_), println(_))


}
