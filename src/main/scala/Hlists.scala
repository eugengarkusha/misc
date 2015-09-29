import java.util.Date

/**
 * Created by admin on 1/17/15.
 */

object Hlists extends App{
  trait HL

  trait H0  extends HL

  object H0 extends H0

  case class :: [H,T<:HL](head:H,tail:T) extends HL

  implicit class HListOps[L<:HL](l:L){
    def ::[H](h:H):H::L = Hlists.::(h,l)
    def head(implicit ev:isHC[L])=ev.head(l)
    def tail(implicit ev:isHC[L])=ev.tail(l)
    def last(implicit ev:Last[L])=ev.last(l)
    def apply[N<:Nat](n:N)(implicit ev:ElLoc[L,N])=ev.loc(l)
    def apply[N<:Nat](implicit ev:ElLoc[L,N])=ev.loc(l)
  }
  abstract class isHC[L<:HL]{
    type He
    type Ta<:HL
    def head(l:L):He
    def tail(l:L):Ta
  }

  object isHC{
    implicit def hc[H,T<:HL]=new isHC[::[H,T]]{
      type He=H
      type Ta=T

      def head(l: He::Ta)=l.head
      def tail(l: He::Ta)=l.tail
    }
  }
  abstract class Last[T<:HL]{
    type R
    def last(l:T):R
  }

  object Last{
    implicit def hlistLast[H,T<:HL](implicit ev:Last[T])=new Last[H::T]{
      type R=ev.R
      def last(l:H::T):R =ev.last(l.tail)
    }
    //canot directly state Last[H::H0]because H0.type(object) matched here as T
    implicit  def hlistSingleLast[H,T<:H0] =new Last[H::T]{
      type R=H
      def last (l:H::T):R=l.head
    }
  }

  trait Nat
  trait _0 extends Nat
  object _0 extends _0
  trait Succ[N<:Nat]extends Nat
  case class Successor[N<:Nat](n:N) extends Succ[N]



  trait ElLoc[L<:HL,N<:Nat]{
    type R
    def loc(l:L):R
  }
  object  ElLoc{
    implicit def hlistLoc[H,T<:HL,N<:Nat](implicit ev:ElLoc[T,N])=new ElLoc[H::T ,Succ[N]]{
      override type R=ev.R
      override def loc(l: H::T): R = ev.loc(l.tail)
    }

    implicit def hlistLoc1[H,T<:HL,Z<:_0]=new ElLoc[H::T,Z]{
      override type R=H
      override def loc(l:H::T): R = l.head
    }
  }




  val k =  new Date::"dd"::1 :: H0
  println(k.last)
  println(k.apply[Succ[_0]])
  //Doesnt work: TBD: need to implement int to nat coversion or smth
  //println(k(1))

}
