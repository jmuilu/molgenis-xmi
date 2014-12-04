package xmitools.db.sql

import xmitools.model.XPackage
import xmitools.model.XClassifier
import xmitools.model.UMLModel
import xmitools.model.XMIVersionHandler
import xmitools.model.XProperty
import xmitools.model.XInstance
import scala.xml.Elem
import xmitools.db.emx.EmxInsertHandler

trait InsertHandler {

  def insertPackage(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    parentPackageIdentifier: Option[String], namespaceIdentifier: String);

  def insertEntity(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    typeCv: String, typeQualifierCv: String, packageIdentifier: String, instanceOfIdentifier: Option[String], generalizations: List[String], realizations: List[String]);

  def insertAttribute(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    typeCv: String, typeQualifierCv: String, entityIdentifier: String,
    classifierIdentifier: String, classifierName: Option[String], associationIdentifier: Option[String],
    lowerBound: Option[String], upperBound: Option[String], navigable: Boolean,
    aggregation: Option[String]);

  def close

}

class MDRDbLoader(model: UMLModel, prefix: String, rootId: String,
  contextId: String, contextName: String, contextDesc: String,
  iHandler: InsertHandler) extends ModelLoader(model, prefix, rootId, contextId, contextName, contextDesc) {

  def insertContext(contextId: String, name: String, description: String) = {
    iHandler.insertPackage(pfix(contextId), name, contextId, toOpt(description), None, prefix);
  }

  def insert(pkg: XPackage, rootId: String) = {
    iHandler.insertPackage(pfix(pkg.id), getOpt(pkg.name), pkg.id, comments(pkg), toOpt(pfix(rootId)), prefix)
  }

  def insert(e: XInstance) = {
    val eType = e.entityType //class,interface,association, primitive...
    val eTypeQualifier = "concrete";
    assert(e.parent.isDefined, "Entity do not belong to a package. Entity: " + e.id)
    val pkgId = e.parent.get.id
    val classifier = e.classifier
    iHandler.insertEntity(pfix(e.id), getOpt(e.name), e.id, comments(e), eType, eTypeQualifier, pfix(pkgId), classifier,
      List[String](),List[String]())
  }

  def insert(e: XClassifier) = {
    val eType = e.entityType //class,interface,association, primitive...
    val eTypeQualifier = if (e.isAbstract.getOrElse(false)) "abstract" else "concrete";
    assert(e.parent.isDefined, "Entity do not belong to a package. Entity: " + e.id)
    val pkgId = e.parent.get.id
    val gList = e.generalizationIds.map(x => pfix(x))
    val rList = e.realizationIds.map(x => pfix(x))
    iHandler.insertEntity(pfix(e.id), getOpt(e.name), e.id, comments(e), eType, eTypeQualifier, pfix(pkgId), None, gList, rList)
  }

  def insertPropertyOwnByNonAssociationEntity(p: XProperty) = {
    val (classifierId, classifierName) = if (p.typeIdRef.isDefined) {
      val typeEntity = resolver.resolve(p.typeIdRef.get)
      if (typeEntity.isDefined) {
        (pfix(typeEntity.get.id), typeEntity.get.name )
      } else {
        logger.warn("Type/Classifier of property " + p.id + " not resolved. Type not resolved (it is likely defined in the extensions element). Dummy is is used = " + ModelDB.DUMMY_ID)
        (ModelDB.DUMMY_ID,None)
      }
    } else {
      logger.warn("Type/Classifier of property " + p.id + " not resolved. Dummy id is used  = " + ModelDB.DUMMY_ID)
      (ModelDB.DUMMY_ID,None) 
    }
    val (typeCode, assoIdentifier) = if (p.association.isDefined) ("associationEnd", p.association.get) else ("attribute", "")
    assert(p.parent.isDefined, "Property do not belong to a entinty. Property: " + p.id)
    val entityIdentifier = p.parent.get.id
    
    iHandler.insertAttribute(pfix(p.id), getOpt(p.name), p.id, comments(p), typeCode, "",
      pfix(entityIdentifier), classifierId, classifierName, toOpt(pfix(assoIdentifier)), p.lowerValue, p.upperValue, true, p.aggregation)
  }

  def insertPropertyOwnByAssociationEntity(p: XProperty) = {
    val typeEntity = resolver.resolve(p.typeIdRef.get)
    if (typeEntity.isDefined) {
      assert(typeEntity.get.name.isDefined, "Check type of association end " + p.id);
      val name = deriveRoleNameForAssociationEnd(p, typeEntity.get)
      val (typeCode, assoIdentifier) = if (p.association.isDefined) ("associationEnd", p.association.get) else ("attribute", "")
      assert(!assoIdentifier.isEmpty(), "Association identifier is missing. " + p.id)
      assert(p.parent.isDefined, "Property do not belong to a entinty. Property: " + p.id)
      val associationEntityIdentifier = p.parent.get.id
      assert(assoIdentifier == associationEntityIdentifier, "Check " + associationEntityIdentifier + " " + assoIdentifier)
      val otherEnd = resolver.resolveOtherEndOfBinaryAssociation(associationEntityIdentifier, p.id)
      assert(otherEnd.isDefined)
      assert(otherEnd.get.typeIdRef.isDefined, "Type is missing in other end. Association end is " + otherEnd.get.id)
      val otherEndTypeIeThisEntity = resolver.resolve(otherEnd.get.typeIdRef.get)
      // entity owner which is linked to this property 
      val entityIdentifier = if (otherEndTypeIeThisEntity.isDefined) {
        pfix(otherEndTypeIeThisEntity.get.id)
      } else {
        logger.warn("Type/Classifier of property (via other end as defined in association element). " + p.id + " not resolved. Type not resolved (it is likely defined in the extensions element). Dummy is is used = " + ModelDB.DUMMY_ID)
        ModelDB.DUMMY_ID
      }
      //todo: should the end belong to association only i.e. entityclass_identifier should be a.id
      //now association owned entities are made owned by the UML class (opposite end)
      //identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier, classifierIdentifier, associationIdentifier, 
      //lowerBound, upperBound, navigable, aggregation
      val classifier = resolver.resolve(typeEntity.get.id);
      val classifierName = if (classifier.isDefined) {
        classifier.get.name
      } else {
        None
      }
      iHandler.insertAttribute(pfix(p.id), name, p.id, comments(p), "associationEnd", "",
        entityIdentifier, pfix(typeEntity.get.id), classifierName, toOpt(pfix(associationEntityIdentifier)),
        p.lowerValue, p.upperValue, p.isNavigable.getOrElse(false), p.aggregation )
    } else {
      logger.warn("Type of property " + p.id + " not resolved")
    }
  }

  def close() = {
    iHandler.close
  }
}

object MDRDbLoader {

  def apply(file: String, prefix: String, submissionIdSource: String, contextId: String, contextName: String, contextDesc: String,
    vsHandler: XMIVersionHandler): MDRDbLoader = {
    vsHandler.setRootId(submissionIdSource)
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi, prefix)(vsHandler)
    val iHandler = new MDRDbInsertHanlder()
    return new MDRDbLoader(model, prefix, submissionIdSource, contextId, contextName, contextDesc, iHandler)
  }

  def apply(file: String, prefix: String, submissionIdSource: String, contextId: String, contextName: String, contextDesc: String,
    vsHandler: XMIVersionHandler, fileName: String): MDRDbLoader = {
    vsHandler.setRootId(submissionIdSource)
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi, prefix)(vsHandler)
    val iHandler = new EmxInsertHandler(fileName, model.createResolver) //todo fix hack. The handler should not use resolver. Also apply method is not used properly 
    return new MDRDbLoader(model, prefix, submissionIdSource, contextId, contextName, contextDesc, iHandler)
  }

  def apply(file: String, prefix: String, submissionIdSource: String, contextId: String, contextName: String, contextDesc: String,
    vsHandler: XMIVersionHandler, iHandler: InsertHandler): MDRDbLoader = {
    vsHandler.setRootId(submissionIdSource)
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi, prefix)(vsHandler)
    return new MDRDbLoader(model, prefix, submissionIdSource, contextId, contextName, contextDesc, iHandler)
  }

  def apply(file: String, prefix: String, submissionIdSource: String, contextId: String, contextName: String, contextDesc: String,
    vsHandler: XMIVersionHandler, iHandler: InsertHandler, xmi: Elem, model: UMLModel): MDRDbLoader = {
    return new MDRDbLoader(model, prefix, submissionIdSource, contextId, contextName, contextDesc, iHandler)
  }

  def main(args: Array[String]): Unit = {
    val m = new ModelDB

    m.recreate

  }

}