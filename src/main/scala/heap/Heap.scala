package heap

import scala.util.Random

/**
 * Created by admin on 7/22/15.
 */
class Heap[T](implicit cmp: Ordering[T]) {
  
  var root:Node = Root
  
    class Node(val v: T, var l: Node=Empty,var r: Node=Empty) extends Ordered[Node]{
      var direction = true
      def compare(that:Node) = if(that.v == null) 1 else cmp.compare(this.v, that.v)

      def biggerThen(that:Node) = compare(that) > 0
      def setNode(dir:Boolean, to:Node, node:Node) = if(dir)to.r= node else to.l = node
      def getNode(dir:Boolean)=if(dir)r else l


      def add(n: Node): Node = {
        val k = if (biggerThen(n)) {
          setNode(direction, to = this, node = getNode(direction).add(n))
          direction = !direction
          this
        } else {
          setNode(!direction,to = n, node = getNode(!direction))
          val nd = getNode(direction )
          this.r = Empty
          this.l = Empty
          setNode(direction,to = n, node = nd.add(this))
          n.direction = !direction
          n
        }
        k
      }

      def remove:Node = {
        if (r.biggerThen(l)) {
          r.r =r.remove
          r.l = l
          r
        } else {
          l.l =l.remove
          l.r = r
          l
        }
      }



      def height:Int={
        val rh = r.height
        val lh = l.height
        1+{if(rh>lh)rh else lh}
      }

      def prnt(nodeLen:Int=1)= {
        val ht = height
        val fmt= s"%${nodeLen}s"
        val sep = " " * nodeLen

        prt(Vector(this))

        def prt(buf: Vector[Node],lvl:Int=ht): Unit = {
          if (lvl > 0) {
            val interval = Math.pow(2, lvl-1).toInt
            print(sep * (interval - 1));
            for (i <- 0 until buf.size) {
              print(Option(buf(i).v).map(fmt.format(_)).getOrElse(fmt.format("N")) )
              print(sep * (interval * 2 - 1))
            }
            println
            prt( buf.foldLeft(Vector.empty[Node]) {
              case (b, Empty) => b :+ Empty :+ Empty
              case (b, n: Node) => b :+ n.l :+ n.r
            },lvl - 1)
          }
        }
      }
      override def toString = s"Node($v, $l, $r)"

    }

  class E extends Node(null.asInstanceOf[T],null,null){
    override def add(n:Node)=n
    override def compare(that:Node) = -1
    override  def height = 0
    override def remove = Empty
  }
  object Empty extends E{
    r=this;l=this

  }
  object Root extends E{
    r=Empty;l=Empty
    override def add(n:Node)={
      root = n
      n
    }
  }

  def remove(): Option[T] = {
    val ret = root.v
    root = root.remove
    Option(ret)
  }
  def add(t:T)= root = root.add(new Node(t))

  //val root

}
object h extends App{
  val hip = new Heap[Int]()
  val s =  (1 to 2).toSet
  println(s)
  s.map{_ =>
    val e = 0//Random.nextInt(3+1)
    hip.add(e)
    println(s"added=$e")
    hip.root.prnt(2)
  }
  (1 to 2).map{ _ =>
    println("RMV="+hip.remove())
    hip.root.prnt(2)
  }

  //println(hip.root.height)


}

