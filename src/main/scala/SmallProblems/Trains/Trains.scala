package common

/**
 * Created by 2 on 07.11.2014.
 */
object Trains extends App {
val Acode=65;

  val str="AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7"
  val input=str.split(", ").map(_.toList).map{
    case from::to::weight::Nil=>(from.toInt-Acode ,to.toInt-Acode, weight.getNumericValue)
  }.groupBy(_._1)

 val createdNodes=Array.ofDim[Graph](str.replaceAll("\\d","").distinct.size)//Map[Int,Graph]()

 case  class Graph(val index:Int){

    def buildChildernOf(i:Int):Seq[(Int,Graph)]={
      input(i).map { case (_,index, weight) => weight -> Option(createdNodes(index)).getOrElse(Graph(index))}
    }
    
    createdNodes(index)=this
    val children=buildChildernOf(index)
    override def toString="ind="+index.toString + " chWeights"+children.map(_._1)
  }

  println(Graph(0).children)



}
