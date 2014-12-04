package xmitools.db.sql

import xmitools.exporters.DataCleaner
import xmitools.model.XMIVersionHandler
import xmitools.model.UMLModel
import xmitools.model.XPackage
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import xmitools.model.XClassifier
import xmitools.model.XNode
import xmitools.model.XAssociation
import xmitools.model.XProperty
import xmitools.model.XInstance

abstract class ModelLoader(val model: UMLModel, val prefix: String, val rootId: String,
  val contextId: String , val contextName: String, val contextDesc: String) extends DataCleaner {

  lazy val resolver = model.createResolver

  val entityMap = new HashMap[String, String]()

  def insertContext( contextId : String, name: String, description:String)
  
  def insert(pkg: XPackage, rootIdentifier: String)

  def insert(e: XClassifier)

  def insert(e: XInstance)

  def insertPropertyOwnByNonAssociationEntity(p: XProperty)

  def insertPropertyOwnByAssociationEntity(p: XProperty)

  def pfix(id: String): String = {
    resolver.pfix( id)
  }

  def load(pkg: XPackage, rootIdentifier: String): Unit = {

    insert(pkg, rootIdentifier)
    (pkg.subPackages).foreach {
      p =>
        assert(p.parent.isDefined)
        assert(pkg.id == p.parent.get.id)
        load(p, pkg.id)
    }

  }

  def loadEntity(e: XClassifier): Unit = {
    if (e.isInstanceOf[XAssociation]) {
      val a = e.asInstanceOf[XAssociation]
      if (a.memberEndIdRefs.size == 2) {
        a.ownedEnds.foreach(oe => insertPropertyOwnByAssociationEntity(oe))
        insert(e)
      } else {
        logger.warn("Can only handle binary relationships. Association: " + a.id + ".  Number of members: " + a.memberEndIdRefs.size)
      }
    } else {
      e.attributes.foreach(p => insertPropertyOwnByNonAssociationEntity(p))
      if ( e.isInstanceOf[XInstance]) {
        insert(e.asInstanceOf[XInstance])
      } else 
        insert(e)
    }
  }


  def loadAll() = {   
    insertContext(contextId, contextName, contextDesc)
    load(model, contextId);
    loadEntities(model)
  }

  def loadEntities(pkg: XPackage) {

    val entities = (pkg :: pkg.allSubPackages).flatMap(p => p.entities)
    entities.foreach { e => loadEntity(e); }

  }


  def comments(e: XNode): Option[String] = {
    val comm = resolver.commentsAsText(e.id)
    return toOpt(comm)
  }

}

//object DBLoader {
//  
//  def apply(file: String, prefix: String, submissionId: String, submissionIdSource: String, contextId: String) = {
//    implicit val vsHandler = XMIVersionHandler.EA_XMI241;
//    val xmi = scala.xml.XML.loadFile(file)
//    val model = UMLModel(xmi)(vsHandler)
//    new DBLoader( model)    
//  }  
//}