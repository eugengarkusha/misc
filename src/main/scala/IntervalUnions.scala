/**
 * Created by admin on 5/28/15.
 */
object Task3_IntervalUnions extends App{
  val a = Array(1, 12, 42, 70, 36, -4, 43, 15)
  val b = Array(5, 15, 44, 72, 36, 2, 69, 24)

  def solution(A: Array[Int], B: Array[Int]): Int = {

    val overlap:((Int,Int),(Int,Int),Int) => ((Int,Int),Int) = {
      case((prevMin, prevMax), (nextMin, nextMax), count) if prevMax >= nextMin =>
        val end = if(prevMax > nextMax) prevMax else nextMax
        prevMin -> end -> count
      case (_ ,next,count) => next -> (count + 1)
    }

    A.zip(B).sortBy(_._1) match {
      case Array(head, tail@_*) => tail.foldLeft(head -> 1) {
        case ((prev, count), next) => overlap(prev, next, count)
      }._2
      case _ => 0
    }
  }
  println(solution(a,b))


}
