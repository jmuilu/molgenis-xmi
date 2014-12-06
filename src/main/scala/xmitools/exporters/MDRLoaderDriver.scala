package xmitools.exporters

import xmitools.model.XMIVersionHandler
import com.typesafe.config.ConfigFactory
import java.io.File
import xmitools.db.emx.EmxTags
import scala.collection.JavaConversions.asScalaBuffer
import xmitools.db.sql._

class MDRLoaderDriver {
  import scala.collection.JavaConversions._

  //    MDRDbLoader("src/test/resources/models/TEST_MODEL_UML241.xmi", "MOD001", 
  //   "submission1", "context1", "Test model 1", "Model used for testing", XMIVersionHandler.EA_XMI241).loadAll()

  def resolveXMIHander(handlerId: String): XMIVersionHandler = {
    handlerId match {
      case "EA_XMI241" => XMIVersionHandler.EA_XMI241;
      case "EA_XMI21" => XMIVersionHandler.EA_XMI21;
      case "ECLIPSE_20110701" => XMIVersionHandler.ECLIPSE_20110701
      case "ECLIPSE_2.0" => XMIVersionHandler.ECLIPSE_2_0
      case "ECLIPSE_2_1" => XMIVersionHandler.ECLIPSE_2_1
            case _ => throw new RuntimeException("Unknown hander " + handlerId);
    }
  }

  def load(fileName: String) = {
    
    val file = new File(fileName)
    val cfg = ConfigFactory.parseFile(file)
    if (cfg.getBoolean("rebuildDb")) {
      println("Rebuilding the db")
      val m = new ModelDB
      m.recreate
    }

    val emx = if (cfg.hasPath("emx.prefix")) Some(cfg.getString("emx.prefix")) else None
    val list = cfg.getConfigList("files")
    list.foreach {
      f =>
        println("Loading..." + f.getString("file") + " ")
        if (emx.isDefined) { //todo: fix hack
          val tagfile = EmxTags(cfg.getString("emx.tags") )
          val fileName = emx.get + "_" + f.getString("sourceName") + ".xlsx"
          val loader = MDRDbLoader(f.getString("sourceName"),f.getString("file"), f.getString("namespace"), f.getString("submission"), f.getString("source"),
            f.getString("sourceName"), f.getString("sourceDescription"),
            resolveXMIHander(f.getString("XMI")), fileName, tagfile)
          loader.loadAll()
          loader.close()
        } else {
          val loader = MDRDbLoader(f.getString("sourceName"),f.getString("file"), f.getString("namespace"), f.getString("submission"), f.getString("source"),
            f.getString("sourceName"), f.getString("sourceDescription"),
            resolveXMIHander(f.getString("XMI")))
          loader.loadAll()
          loader.close()
        }
    }
  }
}

object MDRLoaderDriver {
  def apply() = new MDRLoaderDriver

  def main(args: Array[String]): Unit = {
    //assert(args.length == 1,"Usage: MDRLoaderDriver diverfile")

    val drive = MDRLoaderDriver()
    drive.load("src/test/resources/loaderBatch_simple.conf")

  }
}