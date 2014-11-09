package SmallProblems.Trains

import scala.collection.SortedSet
import scala.annotation.tailrec
import java.util
import scala.collection.immutable.Stack
import scala.collection.mutable.ArrayBuffer

/**
 * Created by 2 on 09.11.2014.
 */
class Trains(graph:String) {
  //English alphabet and digits Unicode values matches ASCII
  //so its possible to effectively determine indecies  and digit range by char values
  val A_CODE=65;
  val MIN_DIGIT=48
  val MAX_DIGIT=57
  def ERR= throw new IllegalArgumentException("Unsupported input format")


 // val graph="AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7"
  
  //let format be a bit more liberal for spaces
  val parsed =graph.trim.split("\\s*,\\s+").map(_.toList).map{
    case from::to::weight if weight.forall(c=> c>=MIN_DIGIT && c<=MAX_DIGIT) => (from-A_CODE ,to-A_CODE, weight.mkString.toInt)
    case _ => ERR
  }

  val listSize=parsed.foldLeft(0){case (max,(from,to,_))=>Seq(max,from,to).max}+1

  val adjList=new Array[Iterable[(Int,Int)]](listSize)

  parsed.groupBy(_._1).foreach(indToList=>adjList(indToList._1)=indToList._2.map{case(_,to,weight)=>(to,weight)})



  def calcDirectRouteDistance(route:String)={

    @tailrec
    def calc(route:Iterable[Int],prevPoint:Int,aggregatedWeight:Int):Int= {
      route.headOption.map {
        nextPoint =>
         adjList(prevPoint).toMap.get(nextPoint).map(weight => (route.drop(1), nextPoint, aggregatedWeight + weight))
      } match {
        case Some(Some((route, point, weight))) => calc(route, point, weight)
        case Some(None) => -1
        case _ => aggregatedWeight
      }
    }

    val prsedRt = """^([A-Z]-)+[A-Z]\.?$""".r.findFirstIn(route).map(_.split("-").map(_.charAt(0)-A_CODE)).getOrElse(ERR)
    calc(prsedRt.drop(1),prsedRt.head,0)

  }

  private def getStartEnd(route:String)="""^[A-Z],[A-Z]$""".r.findFirstIn(route).map(s=>(s.charAt(0)-A_CODE,s.charAt(2)-A_CODE)).getOrElse(ERR)

  def calcShortestDistance(route:String)={

    val(start,end)=getStartEnd(route)
    val pathWeights= ArrayBuffer[(Long,Int)]().padTo(listSize,(Int.MaxValue.toLong+1,-1)).toArray

    @tailrec
    def calc(stack:Stack[(Int,Int)]){

     if(stack.nonEmpty){
       val ((start,aggregatedWeight),poppedBuffer)=stack.pop2
       def kids=adjList(start)

       calc {
         kids.map {
           case k@(ind, weight) =>
             val fullWeight = weight + aggregatedWeight
             if (fullWeight < pathWeights(ind)._1) {
               pathWeights(ind) = (fullWeight.toLong, start)
               Some(k)
             } else None
         }.flatten.toList.sortBy(_._2)(implicitly[Ordering[Int]].reverse).foldLeft(poppedBuffer)((s, k) => s.push((k._1, aggregatedWeight + k._2)))
       }
      }
    }

    calc(Stack[(Int,Int)]().push(start->0))
    pathWeights(end)._1
  }

  private def calcWeightWithConditions(findCond:(Int,Int)=>Boolean,searchCond:(Int)=>Boolean,kidsProcessor:((Int,Int),Int)=>Int,subject:Int,route:String)={
    
    val(start,end)=getStartEnd(route)

    def calc(found:Int,stack:Stack[(Int,Int)]):Int= {
      if (stack.isEmpty) found  else{
        val ((start, subj), tail) = stack.pop2
        if (searchCond(subj)) {
          val kids = adjList(start)
          def stackWithKids = kids.foldLeft(tail)((s, k) => s.push((k._1, kidsProcessor(k,subj))))
          def gotIt=kids.toMap.get(end).find(weight=>findCond(weight,subj)).isDefined
          calc(if(gotIt)found+1 else found, stackWithKids)
        } else calc(found, stack.pop)
      }
    }
    calc(0,Stack().push(start->subject))
  }

//  def calcNumberOfRoutesByStops(stops:Int,condition:(Int)=>Boolean)
//  def calcNumberOfRoutesByDistance=Ordering[Int].gt

  println(calcDirectRouteDistance("A-D-C."))
  println(calcShortestDistance("B,E"))
  println(calcWeightWithConditions((_,s)=>s>=0,_>=0,(_,s)=>s-1,2,"C,C"))
  println(calcWeightWithConditions((_,s)=>s==0,_>=0,(_,s)=>s-1,3,"A,C"))
  println(calcWeightWithConditions((w,s)=>s-w>0,_>0,(k,s)=>s-k._2,30,"C,C"))

}
