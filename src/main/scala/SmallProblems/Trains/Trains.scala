package SmallProblems.Trains

import scala.collection.SortedSet
import scala.annotation.tailrec
import java.util
import scala.collection.immutable.Stack
import scala.collection.mutable.ArrayBuffer
import sun.beans.editors.EnumEditor

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
    def calc(route:Iterable[Int],prevPoint:Int,aggregatedWeight:Int):String= {
      route.headOption.map { nextPoint =>
         adjList(prevPoint).toMap.get(nextPoint).map(weight => (route.drop(1), nextPoint, aggregatedWeight + weight))
      } match {
        case Some(Some((route, point, weight))) => calc(route, point, weight)
        case Some(None) => "NO SUCH ROUTE"
        case _ => aggregatedWeight.toString
      }
    }

    val prsedRt = """^([A-Z]-)+[A-Z]\.?$""".r.findFirstIn(route).map(_.split("-").map(_.charAt(0)-A_CODE)).getOrElse(ERR)
    calc(prsedRt.drop(1),prsedRt.head,0)

  }

  private def getStartEnd(route:String)="""^[A-Z],[A-Z]$""".r.findFirstIn(route).map(s=>(s.charAt(0)-A_CODE,s.charAt(2)-A_CODE)).getOrElse(ERR)

  def calcShortestDistance(route:String)={

    val((start,end),infinity)=(getStartEnd(route),Int.MaxValue.toLong+1)
    val pathWeights= ArrayBuffer[(Long,Int)]().padTo(listSize,(infinity,-1)).toArray

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
    pathWeights.lift(end).find(_._1 !=infinity).map(_.toString).getOrElse("NO SUCH ROUTE")
  }
  object Conditions {
    case class Val(get:(Int,Int)=>Boolean){def apply(i:Int,j:Int)=get(i,j)}
    private val ord = implicitly[Ordering[Int]]
    val equiv=Val(ord.equiv)
    val lt=Val(ord.lt)
    val lteq=Val(ord.lteq)
  }

  import Conditions._
  private def calcWeightWithConditions(findCond:(Int,Int)=>Boolean,searchCondition:Conditions.Val,kidsProcessor:(Int,Int)=>Int,threshold:Int,route:String)={

    val(start,end)=getStartEnd(route)

    def calc(found:Int,stack:Stack[(Int,Int)]):Int= {
      if (stack.isEmpty) found  else{
        val ((start, subj), tail) = stack.pop2
          def kids=adjList(start)
          def addKidds = kids.foldLeft(tail){case(s,(kInd,kWeight)) => s.push((kInd, kidsProcessor(kWeight,subj)))}
          def calcFound=kids.toMap.get(end).find(findCond(_,subj)).map(_=>found+1).getOrElse(found)
          val(_stack,_found) = {
            if (searchCondition(subj, threshold)) addKidds -> calcFound
            else if(searchCondition==equiv && subj<threshold)addKidds->found
            else stack.pop->found
          }
          calc(_found,_stack)
      }
    }
    calc(0,Stack().push(start->0)).toString
  }

  def calcNumberOfRoutesByStops(threshold:Int,cond:Val,route:String)={
    calcWeightWithConditions((_,s)=>cond(s,threshold),cond,(_,s)=>s+1,threshold,route)
  }
  def calcNumberOfRoutesByDistance(threshold:Int,cond:Val,route:String)={
    calcWeightWithConditions((w,s)=>cond(w+s,threshold),cond,(w,s)=>s + w,threshold,route)
  }


 /* println(calcDirectRouteDistance("A-D-C-H."))
  println(calcShortestDistance("B,E"))
  println(calcNumberOfRoutesByStops(3,lt,"C,C"))
  println(calcNumberOfRoutesByStops(4,equiv,"A,C"))
  println(calcNumberOfRoutesByDistance(30,lt,"C,C"))
*/



}
