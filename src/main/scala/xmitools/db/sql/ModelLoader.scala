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

abstract class ModelLoader(val model: UMLModel, val prefix: String, val rootId: String) extends DataCleaner {

  val resolver = model.createResolver

  val entityMap = new HashMap[String, String]()

  def insert(pkg: XPackage, rootIdentifier: String)

  def insert(e: XClassifier)

  def insertPropertyOwnByNonAssociationEntity(p: XProperty)

  def insertPropertyOwnByAssociationEntity(p: XProperty)

  def pfix(id: String): String = {
    if (id != null && !id.isEmpty()) return prefix + id;
    else return ""
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
    insert(e)
    if ( e.isInstanceOf[XAssociation]) {
        val a = e.asInstanceOf[XAssociation]
        a.ownedEnds.foreach ( oe => insertPropertyOwnByAssociationEntity(oe))
    } else {
        e.attributes.foreach ( p=> insertPropertyOwnByNonAssociationEntity(p))
    }
  }

  def loadAll() = {
    load(model, rootId);
    loadEntities(model)
  }

  def loadEntities(pkg: XPackage) {

    val entities = (pkg :: pkg.allSubPackages).flatMap(p => p.entities)
    entities.foreach { e => loadEntity(e); }

  }

  def load(file: String, prefix: String, submissionId: String, submissionIdSource: String, contextId: String) {

    implicit val vsHandler = XMIVersionHandler.EA_XMI241;
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi)(vsHandler)

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