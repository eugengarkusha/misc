package SmallProblems.MarsRover

import scala.io.Source

/**
 * Created by 2 on 05.11.2014.
 */
object MarsRover extends App{

val warnMsg="Warning:atempt to move beyond permitted area"
val reader=Source.fromInputStream(System.in).bufferedReader()
  
class Directions( val xLimit:Int,val yLimit:Int) {

  abstract class Direction(id:Int,val name:String) {
    def L(x:Int,y:Int)=(calcDir(-1),(x,y))
    def R(x:Int,y:Int)=(calcDir(1),(x,y))
    def M(x:Int,y:Int)=if(x<=xLimit && x>=0 && y<=yLimit &&y>=0) (this,move(x,y)) else{ println(warnMsg);(this,(x,y))}
    protected def move(x:Int,y:Int):Tuple2[Int,Int]
    private def calcDir(turn:Int)= values((4+id+turn)%4)
    values+=(id->this)
    override def toString=name
  }

  val values=scala.collection.mutable.Map[Int,Direction]()
  def apply(c:String)=values.values.find(_.name==c)

  val N =new Direction(0,"N"){def move(x:Int,y:Int)=(x,y+1)}
  val E =new Direction(1,"E"){def move(x:Int,y:Int)=(x+1,y)}
  val S =new Direction(2,"S"){def move(x:Int,y:Int)=(x,y-1)}
  val W =new Direction(3,"W"){def move(x:Int,y:Int)=(x-1,y)}
}

//user has 1568 tries till StackOverflow with default stackSize so it's no need in @tailrec
  def initDirectionsByUserInput:Directions={
    """^(\d)\s+(\d)$""".r.findFirstMatchIn(reader.readLine.trim).map(bounds=>
      new Directions(bounds.group(1).toInt,bounds.group(1).toInt)).getOrElse{
      println("bad field size syntax, try again ")
      initDirectionsByUserInput
    }
  }
  
  val directions=initDirectionsByUserInput

  while(true) {
    println((for (
      init <- """^(\d)\s+(\d)\s+([NESW]{1})$""".r.findFirstMatchIn(reader.readLine.trim);
      mvmt <- """^[RLM]+$""".r.findFirstIn(reader.readLine.trim)
    ) yield {

     val(x,y,dir)=(init.group(1).toInt,init.group(2).toInt,init.group(3))
     mvmt.foldLeft((directions(dir).get,(x,y))){
       case ((d,(x,y)),'R')=>d.R(x,y)
       case ((d,(x,y)),'L')=>d.L(x,y)
       case ((d,(x,y)),'M')=>d.M(x,y)
     }
    }).map{case (d,(x,y))=>"%s %s %s".format(x,y,d)}.getOrElse("Parameters input is incorrect"))

  }
}
