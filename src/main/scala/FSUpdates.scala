package pr587

import java.nio.file._
import java.io.File
import java.nio.file.StandardWatchEventKinds._
import collection.JavaConverters._
import com.sun.nio.file.SensitivityWatchEventModifier
import groovy.lang.{GroovyShell}
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

//ScriptCache instance consumes one thread!
//if FS event monitoring is needed in other places , use https://github.com/lloydmeta/schwatcher
class ScriptsCache(scriptsPath: Path) {

  def scripts: collection.Map[String, String] = _scripts

  private var _scripts = Map[String,String]()

  private val valid = {
    val gs =  new GroovyShell
    (s:String)=> Try(gs.parse(s)).isSuccess
  }

  private def populateCache(e: WatchEvent.Kind[_], script:File): Unit = {

    val name = script.getName

    def logAbort(msg:String) = println(s"abort($name):$msg")
    def logSuccess(msg:String) = println(s"success($name):$msg")

    println(s"fs event $e on file $name")

    if (e == ENTRY_DELETE) {
      if(_scripts.contains(name))logSuccess("script is removed")
      _scripts = _scripts - name
    }
    else if (e == ENTRY_MODIFY){
      if (!name.endsWith(".groovy")) logAbort("script name has to end with .groovy")
      else if (script.length > 20e6) logAbort("script size must be less then 20 MB")
      else{
       Some(Source.fromFile(script).mkString).filter(valid).map{body =>
         _scripts = _scripts + (name ->  body)
         logSuccess("script is added/updated")
       }.getOrElse(logAbort(s"script file contains invalid groovy script ${if(scripts.contains(name))"keeping previous version" else ""}"))
      }
    }
    else println(s"unexpected filesystem event $e")

  }

  println(s"adding existing scripts from ${scriptsPath} to cache")
  scriptsPath.toFile.listFiles.filter(_.isFile).foreach(populateCache(ENTRY_MODIFY, _))
  println(s"finished populating cache")

  Future{

    def start: Unit = {
      val watchService = FileSystems.getDefault.newWatchService()

      if (System.getProperty("os.name").startsWith("Mac"))
        scriptsPath.register(watchService,
          Array(ENTRY_MODIFY, ENTRY_DELETE).asInstanceOf[Array[java.nio.file.WatchEvent.Kind[_]]],
          SensitivityWatchEventModifier.HIGH)
      else scriptsPath.register(watchService, ENTRY_MODIFY, ENTRY_DELETE)

      while(true) try{
        val key = watchService.take()
        key.pollEvents().asScala.foreach{ e => populateCache(e.kind, scriptsPath.resolve(e.context.toString).toFile)}
        key.reset()
      } catch {case t:Throwable => println(s"scripts watch service encountered $t. Restarting service"); start}
    }

    println(s"starting tracking scripts directory: $scriptsPath")
    start
  }

}
object FileMod3  extends App {
  val sc = new ScriptsCache(Paths.get("/tmp"))
  println(sc.scripts)
  Thread.sleep(15300)
  println(sc.scripts)
}
