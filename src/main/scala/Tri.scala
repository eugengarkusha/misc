/**
 * Created by admin on 5/13/15.
 */

object Tri extends App {


  val s: Stream[List[Int]]  = List(1, 1) #:: s.map(l=> (1 :: l.sliding(2,1).map(_.reduce(_ + _)).toList) :+  1)

  def pascal (c:Int,r:Int)= {
    if (r == 0) 1
    else s(r - 1).lift(c).getOrElse(1)
  }
//  Array.empty[Any].foldRight() //-> def foldRight[B](z: B)(op: (A, B) => B): B =
//                               //   foldr(0, length, z, op) ->  @tailrec
//                                                               //private def foldr[B](start: Int, end: Int, z: B, op: (A, B) => B): B =
//                                                               //if (start == end) z
//                                                               //else foldr(start, end - 1, op(this(end - 1), z), op)
//
//
//  Nil.foldRight() //-> override def foldRight[B](z: B)(op: (A, B) => B): B =
//                  //   reverse.foldLeft(z)((right, left) => op(left, right))

  println{
    pascal(0,2) ::
    pascal(1,2) ::
    pascal(2,3) ::
    pascal(2,4) :: Nil
  }// returns List(1, 2, 3, 6)


}
