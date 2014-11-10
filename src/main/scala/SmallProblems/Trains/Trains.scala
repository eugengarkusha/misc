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

class Trains(val graph:String) {
  //English alphabet and digits Unicode values matches ASCII
  //so its possible to effectively determine indecies  and digit range by char values
  val A_CODE=65;
  val MIN_DIGIT=48
  val MAX_DIGIT=57
  def ERR(msg:String="Unsupported input format")= throw new IllegalArgumentException(msg)

  //let format be a bit more liberal for spaces
  val parsed =graph.trim.split("\\s*,\\s+").map(_.toList).map{
    case from::to::weight if weight.forall(c=> c>=MIN_DIGIT && c<=MAX_DIGIT) => (from-A_CODE ,to-A_CODE, weight.mkString.toInt)
    case _ => ERR()
  }

  //input string additional checks
  {val fromTo=parsed.map{case(from,to,_)=>(from,to)}
    if(fromTo.distinct.size!=parsed.size)ERR("Duplicate routed detected")
    if(fromTo.exists{case(from,to)=>from==to})ERR("Loop routes detected")}



  //size is determined by the index of the element with maximal code(its expected that node labels sequence is consistent)
  val listSize=parsed.foldLeft(0){case (max,(from,to,_))=>Seq(max,from,to).max}+1
  //Adjacency list to represent graph
  val adjList=new Array[Iterable[(Int,Int)]](listSize)

  //populating the adjacency list
  parsed.groupBy(_._1).foreach(indToList=>adjList(indToList._1)=indToList._2.map{case(_,to,weight)=>(to,weight)})


  def calcDirectRouteDistance(route:String)={

    @tailrec
    def calc(route:Iterable[Int],prevPoint:Int,aggregatedWeight:Int):String= {
      route.headOption.map { nextPoint =>
         adjList(prevPoint).find(_._1==nextPoint).map{case(_,weight) => (route.drop(1), nextPoint, aggregatedWeight + weight)}
      } match {
        case Some(Some((route, point, weight))) => calc(route, point, weight)
        case Some(None) => "NO SUCH ROUTE"
        case _ => aggregatedWeight.toString
      }
    }

    val prsedRt = """^([A-Z]-)+[A-Z]\.?$""".r.findFirstIn(route).map(_.split("-").map(_.charAt(0)-A_CODE)).getOrElse(ERR())
    calc(prsedRt.drop(1),prsedRt.head,0)

  }

  private def getStartEnd(route:String)="""^[A-Z],[A-Z]$""".r.findFirstIn(route).map(s=>(s.charAt(0)-A_CODE,s.charAt(2)-A_CODE)).getOrElse(ERR())

  //Dijkstra’s algorithm is applied here
  def calcShortestDistance(route:String)={

    val((start,end),infinity)=(getStartEnd(route),Int.MaxValue.toLong+1)
    //pathWeights holds not only the weights for noedes by index but
    //the previous node on a shortest path so it is easy to get it if needed.
    val pathWeights= ArrayBuffer[(Long,Int)]().padTo(listSize,(infinity,-1)).toArray

    @tailrec
    //indsToAggrWeights is a buffer for aggregating children of newely visited nodes for further processing
    def calc(indsToAggrWeights:List[(Int,Int)]){

     if(indsToAggrWeights.nonEmpty){
       val ((start,aggregatedWeight),tail)=(indsToAggrWeights.head,indsToAggrWeights.tail)
       def kids=adjList(start)
       if(kids!=null)calc {
         kids.map {
           case k@(ind, weight) =>
             val fullWeight = weight + aggregatedWeight
             if (fullWeight < pathWeights(ind)._1) {
               pathWeights(ind) = (fullWeight.toLong, start)
               Some(k)
             } else None
         }.flatten.toList.sortBy(_._2)(implicitly[Ordering[Int]].reverse).foldLeft(tail){
           case(tail, (kInd,kWeight)) => (kInd, aggregatedWeight + kWeight)::tail}
       }else calc(tail)
      }
    }

    calc(List(start->0))
    pathWeights.lift(end).find(_._1 !=infinity).map(_._1.toString).getOrElse("NO SUCH ROUTE")
  }

  object Conditions {
    abstract class Val(get:(Int,Int)=>Boolean){ def threshold:Int;def apply(subj:Int)=get(subj,threshold)}
    private val ord = implicitly[Ordering[Int]]
    case class Eq(threshold:Int)extends Val(ord.equiv)
    case class <(threshold:Int)extends Val(ord.lt)
    case class <=(threshold:Int)extends Val(ord.lteq)
  }

  import Conditions._
  //this method covers relatively similar cases 6 , 7 and 10.
  //subjectProcessor - transforms subject of calculation based on nodes weight and current subject of calculation (weight is ignored in case of stops calculation)
  private def calcWeightWithConditions(searchCond:Val,subjProcessor:(Int,Int)=>Int,route:String)={

    val(start,end)=getStartEnd(route)
    //indToSubj is the aggregator similar to one used in calcShortestDistance
    //ind - node index, subj- subject of calculation(weight or path length)
    def calc(indToSubj:(Int,Int)):Int= {
        val (start, subj)= indToSubj
        val kids=adjList(start).map{case(kInd,kWeight) => (kInd, subjProcessor(kWeight,subj))}.filterNot(_._2 > searchCond.threshold)
        val calcFound=kids.find(_._1==end).find(k=>searchCond(k._2)).map(_=>1).getOrElse(0)
        if(kids.isEmpty)0 else{kids.par.map(k=>calc(k)).reduce(_+_)+calcFound}
    }
    calc(start->0).toString
  }

  def calcNumberOfRoutesByStops(cond:Val,route:String)={
    calcWeightWithConditions(cond,(_,s)=>s+1,route)
  }
  def calcNumberOfRoutesByDistance(cond:Val,route:String)={
    calcWeightWithConditions(cond,(w,s)=>s + w,route)
  }




}
