/**
 * Created by admin on 7/9/15.
 */
package scalazvalidation
import java.io.{PrintWriter, FileWriter, BufferedWriter, File}
import scala.io.Source
import scalaz.Scalaz._
import scalaz.{ValidationNel, Failure, Success}

object Swiripa {

  def createFile(path:String):ValidationNel[String,Iterator[String]]={
    val f = new File(path);
    if(f.exists() && f.canRead)Source.fromFile(f).getLines().successNel else s"file not found: $path".failureNel
  }

  val getAddrs: Iterator[String] => Set[String] =  _.foldLeft(Set.empty[String]->""){
     case ((buf, leftover), nextLine) =>
         val l = (leftover + nextLine).toLowerCase.replaceAll("""[^-a-z0-9_\+\.@ ]"""," ")
         buf ++  mailRx.findAllIn(l) -> l.drop(l.lastIndexOf(' '))
   }._1


  def main(args:Array[String]): Unit ={

      (createFile("addrs2Chk.txt")|@| createFile("contacts.txt")){(a,b)=>
        if(args.headOption.exists(_== "merge"))getAddrs(a) ++ getAddrs(b)
        else getAddrs(a).diff(getAddrs(b))
      } match{
      case Success(s) => val wr = new PrintWriter(new FileWriter("result.txt"),true); s.foreach(wr.println);wr.close
      case Failure(f) => println(f.toList.mkString("\n"))
    }
  }

val mailRx = """([a-z0-9][-a-z0-9_\+\.]*[a-z0-9])@([a-z0-9][-a-z0-9\.]*[a-z0-9]\.(arpa|root|aero|biz|cat|com|coop|edu|gov|info|int|jobs|mil|mobi|museum|name|net|org|pro|tel|travel|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|ee|eg|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)|([0-9]{1,3}\.{3}[0-9]{1,3}))""".r

}
