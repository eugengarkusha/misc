import java.util

import scala.util.Random

/**
 * Created by admin on 4/24/15.
 */
import scala.util.Random.nextInt

object Ants extends App {

  new Field(20,20,1,15).start

import Field.{CoordinateType,_}

  object AntType extends Enumeration{ val Blue, Green, Black = new Val }


    class Field(xDim:Int, yDim:Int, antsSetup:Seq[Tuple3[AntType.Value, Int, Int]], grainCoordinates: Seq[Tuple2[ Int, Int]]) {

      private val grains = collection.mutable.Set(grainCoordinates: _*)
      if(grains.size < grainCoordinates.size) throw new IllegalArgumentException("Duplicate grain coordinates detected")
      if(antsSetup.isEmpty || antsSetup.size > 10) throw new IllegalArgumentException("only 1 to 10 ants is allowed")

      val ants = antsSetup map{
        case (_type, x, y) => _type match{
          case AntType.Blue => new BlueAnt(x,y)
          case AntType.Green => new GreeAnt(x,y)
          case AntType.Black => new BlackAnt(x,y)
        }
      } toVector

      def this(x: Int, y: Int, antCount: Int, grainDensity: Float) = this(
        xDim = x,
        yDim = y,
        antsSetup = (1 to antCount) map (_ => (AntType(nextInt(3)), nextInt(x), nextInt(y))),
        grainCoordinates = {
          val buf = collection.mutable.Set.empty[(Int, Int)]
          (1 to Math.ceil(x * y * grainDensity / 100).toInt) foreach  (_ => while (!buf.add(nextInt(x) -> nextInt(y))) {})
          buf.toSeq
        }
      )

      def start: Unit ={
        val s = grains.toSet
        println(s"grains size =${grains.size} Ants=+$ants\n Grains=${grains.toList.sorted}")
        (0 to 10) map{i=>
         // print(i+":")
          ants(i % ants.size).move

        }
        println("Finish!")
        println(s"grains size =${grains.size} Ants=+$ants\n Grains=${grains.toList.sorted}")
      }

      abstract class Ant(var coordinates: Tuple2[Int, Int], var withGrain: Boolean) {

        override def toString = s"${this.getClass.getSimpleName}:x=$xPos,y=$yPos,withGrain=$withGrain"

        def move: Unit

        def xPos = coordinates._1

        def yPos = coordinates._2

        def dropGrain: Unit = {
         // println("   dropping grain")
          withGrain = false
          grains += (coordinates)
        }

        def collectGrain = {
          //println("   collecting grain")
          withGrain = true
          grains.remove(coordinates)
        }

        def directions = {
          def getCoordinateType(c: Int, max: Int) = {
            if (c == 0) CoordinateType.min else if (c == max) CoordinateType.max else CoordinateType.mid
          }
          getAdjacentPaths(getCoordinateType(xPos, xDim - 1) -> getCoordinateType(yPos, yDim - 1))
        }

        protected def adjacentFields(_directions: List[(Int, Int)] = directions) =
          _directions map { case (xIncr, yIncr) => (xPos + xIncr, yPos + yIncr)}

      }

      class BlueAnt(x: Int, y: Int, grain: Boolean = false) extends Ant(x -> y, grain) {


        def move = move()

        protected def move(fields: List[(Int, Int)] = adjacentFields()): Unit = {
          //val(xx,yy)= (xPos,yPos)
          coordinates = fields(Random.nextInt(fields.size))
          handleGrain(fields)
        // print(s"${this.getClass.getSimpleName} is moving from $xx:$yy to");println(s"$xPos:$yPos")
        }

        def handleGrain(fields: List[(Int, Int)]): Unit = {
          if (withGrain && !grains.contains(coordinates) && fields.filter(grains.contains).length > 1) dropGrain
          else if (!withGrain && grains.contains(coordinates)) collectGrain
        }
      }

      class GreeAnt(x: Int, y: Int, grain: Boolean = false) extends BlueAnt(x, y, grain) {
        override def move(fields: List[(Int, Int)] = adjacentFields()) = {
         // println("moving :Fields ="+fields)
          lazy val fieldWithGrain=fields.find(grains.contains(_));
          if (!withGrain && fieldWithGrain.isDefined) {
           // println("enternig !withgr area wg="+withGrain)
           // val(xx,yy)= (xPos,yPos)
            fieldWithGrain.foreach { f =>
              coordinates = f
              collectGrain
            }
          //  print(s"${this.getClass.getSimpleName} is moving from $xx:$yy to");println(s"$xPos:$yPos")
          } else super.move(fields)
        }
      }

      class BlackAnt(x: Int, y: Int, grain: Boolean = false) extends BlueAnt(x, y, grain) {
        var prevDirection: Tuple2[Int, Int] = 0 -> 0

        override def move = {
          val savedCoordinates = coordinates
          val fields = adjacentFields(directions.filterNot(_ == prevDirection))
          lazy val grainsFree = fields.filterNot(grains.contains(_))
          if (!withGrain || grainsFree.isEmpty) super.move(fields)
          else super.move(grainsFree)

          prevDirection = savedCoordinates match {
            case (x, y) => (xPos - x, yPos - y)
          }
        }
      }
    }
    object Field {

      private val (anyIncr, lowLinitIncr, hiLimitIncr) = (List(-1, 1, 0), List(1, 0), List(-1, 0))

      object CoordinateType extends Enumeration {val min,mid,max = new Val}
      import CoordinateType._

      def combineWithPos[T](l1:List[T],l2:List[T])= l1.map(x=> l2.map(y=> x->y)).flatten

      private def l = List(min,mid,max)

      private def combinations = combineWithPos(l,l)

      def getAllowed(c:CoordinateType.Value) ={
        if(c == min) lowLinitIncr else if(c == max) hiLimitIncr else anyIncr
      }

      val getAdjacentPaths = combinations zip combinations.map {//init called to exclude 0->0 combination
        case (xPos, yPos) => combineWithPos(getAllowed(xPos), getAllowed(yPos)).init
      } toMap


    }
    



}
