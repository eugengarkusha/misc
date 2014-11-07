package NatNum

/**
 * Created by 2 on 03.08.2014.
 */
abstract class Nat {
  def +(n:Nat):Nat
  def -(n:Nat):Nat
  def prev:Nat
  def next:Nat=new Succ(this)
  def isZero:Boolean
  def *(n:Nat):Nat
  def ~(n:Nat)=this*Ten+n



}





object Zero extends Nat{
   def prev =throw new IllegalStateException("going negative")
   def isZero=true
   def +(n:Nat):Nat=n
   def -(n:Nat):Nat=if(n==Zero)n else throw new IllegalStateException("going negative")
   def *(n:Nat)=Zero
  override  def toString="0"

}
class Succ(predecessor:Nat)extends Nat{
  def prev=predecessor;
  def +(n:Nat):Nat= new Succ(n+predecessor)
  def -(n:Nat):Nat=if(!n.isZero)predecessor-n.prev else this
  def isZero=false
  def *(n:Nat)=n+predecessor*n

}
object One extends Succ(Zero)
object Two extends Succ(One)
object Three extends Succ(Two)
object Four extends Succ(Three)
object Five extends Succ(Four)
object Six extends Succ(Five)
object Seven extends Succ(Six)
object Eight extends Succ(Seven)
object Nine extends Succ(Eight)
object Ten extends Succ(Nine)

object main extends App{
  val k = (One~Eight*Two*Three)~Five - One~Zero~Zero~One

  def count(i:Nat,cnt:Int=0):Int=if(i.isZero)cnt else count(i.prev,cnt+1)
  println(count(k))
}



