package xmitools.db.sql

import xmitools.model.XPackage
import xmitools.model.XClassifier
import xmitools.model.UMLModel
import scala.slick.driver.MySQLDriver.simple._
import xmitools.model.XMIVersionHandler
import xmitools.model.XProperty
import java.beans.Introspector
import sun.security.pkcs11.Secmod.DbMode
import xmitools.model.XInstance

class DbLoader(model: UMLModel, prefix: String, rootId: String, 
    contextId: String , contextName: String, contextDesc: String) extends ModelLoader(model, prefix, rootId, contextId, contextName, contextDesc) {
  val DB = new ModelDB

  def insertContext( name: String,  contextId : String, description:String) = {
    DB.DB.withSession {
      implicit session =>
        /* 
       * name, identifier, sourceIdentifier, description, packageIdentifier 
       */
        DB.packages += ( name, pfix(contextId), contextId, toOpt(description), None, prefix );  
    }   
  }
  
  def insert(pkg: XPackage, rootId: String) = {
    DB.DB.withSession {
      implicit session =>
        /* 
       * name, identifier, sourceIdentifier, description, packageIdentifier 
       */
        DB.packages += (getOpt(pkg.name), pfix(pkg.id), pkg.id, comments(pkg), toOpt(pfix(rootId)), prefix)
    }
  }

  def insert(e: XInstance) = {
    DB.DB.withSession {
      implicit session =>
        val eType = e.entityType //class,interface,association, primitive...
        val eTypeQualifier = "concrete";
        assert(e.parent.isDefined, "Entity do not belong to a package. Entity: " + e.id)
        val pkgId = e.parent.get.id
        val classifier = e.classifier
        /* 
        * identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, packageIdentifier
        */
        DB.entities += (pfix(e.id), getOpt(e.name), e.id, comments(e), eType, eTypeQualifier, pfix(pkgId), Some(classifier))
    }
  }

  def insert(e: XClassifier) = {
    DB.DB.withSession {
      implicit session =>
        val eType = e.entityType //class,interface,association, primitive...
        val eTypeQualifier = if (e.isAbstract.getOrElse(false)) "abstract" else "concrete";
        assert(e.parent.isDefined, "Entity do not belong to a package. Entity: " + e.id)
        val pkgId = e.parent.get.id
        /* 
        * identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, packageIdentifier
        */
        DB.entities += (pfix(e.id), getOpt(e.name), e.id, comments(e), eType, eTypeQualifier, pfix(pkgId), None)

        e.parentIds.foreach { x =>
          DB.entityExtends += (pfix(e.id), pfix(x))
        }
    }
  }

  def insertPropertyOwnByNonAssociationEntity(p: XProperty) = {
    DB.DB.withSession {
      implicit session =>
        /*
       * identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier, typeIdentifier, associationIdentifier, lowerBound, upperBound,navigable
       */
        val classifierId = if (p.typeIdRef.isDefined) {
          val typeEntity = resolver.resolve(p.typeIdRef.get)
          if (typeEntity.isDefined) {
            pfix(typeEntity.get.id)
          } else {
            logger.warn("Type/Classifier of property " + p.id + " not resolved. Type not resolved (it is likely defined in the extensions element). Dummy is is used = " + ModelDB.DUMMY_ID)
            ModelDB.DUMMY_ID
          }
        } else {
          logger.warn("Type/Classifier of property " + p.id + " not resolved. Dummy id is used  = " + ModelDB.DUMMY_ID)
          ModelDB.DUMMY_ID
        }
        val (typeCode, assoIdentifier) = if (p.association.isDefined) ("associationEnd", p.association.get) else ("attribute", "")
        assert(p.parent.isDefined, "Property do not belong to a entinty. Property: " + p.id)
        val entityIdentifier = p.parent.get.id
        DB.attributes += (pfix(p.id), getOpt(p.name), p.id, comments(p), typeCode, "",
          pfix(entityIdentifier), classifierId, toOpt(pfix(assoIdentifier)), p.lowerValue, p.upperValue, true, p.aggregation)

    }
  }

  def insertPropertyOwnByAssociationEntity(p: XProperty) = {
    DB.DB.withSession {
      implicit session =>
        /*
       * identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier, typeIdentifier, associationIdentifier, lowerBound, upperBound,navigable
       */
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
          DB.attributes += (pfix(p.id), name, p.id, comments(p), "associationEnd", "",
            entityIdentifier, pfix(typeEntity.get.id), toOpt(pfix(associationEntityIdentifier)),
            p.lowerValue, p.upperValue, p.isNavigable.getOrElse(false), p.aggregation)
        } else {
          logger.warn("Type of property " + p.id + " not resolved")
        }

    }
  }

}

object DbLoader {

  def apply(file: String, prefix: String, submissionIdSource: String, contextId:String, contextName:String, contextDesc:String,
      vsHandler: XMIVersionHandler): DbLoader = {
    vsHandler.setRootId(submissionIdSource)
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi)(vsHandler)
    return new DbLoader(model, prefix, submissionIdSource,contextId, contextName, contextDesc )
  }

  def main(args: Array[String]): Unit = {

      val xmi = scala.xml.XML.loadFile("src/test/resources/models/FuGE-v1-profile.mdxml")
    val model = UMLModel(xmi)(XMIVersionHandler.MD_XMI21);
    model.allPackages.foreach { 
      x =>  println(x.id+" "+x.name) 
      x.subPackages.foreach{
        s=>
        println("Sub "+s.id)  
      }
      x.allChildren.foreach {
        c=>
        println("  "+c.id+" "+c.name)  
      }
    }   
  
    val m = new ModelDB
//    m.recreate

//    DbLoader("src/test/resources/models/TEST_MODEL_UML241.xmi", "MOD001", "submission1", "context1", "Test model 1","Model used for testing", XMIVersionHandler.EA_XMI241).loadAll()
//    DbLoader("src/test/resources/models/BRIDG_UML241.xmi", "MOD002", "submission2",  "context2", "BRIDG","BRIDG - The Biomedical Research Integrated Domain Model",XMIVersionHandler.EA_XMI241).loadAll()
//    DbLoader("src/test/resources/models/model_reg.uml", "MOD003", "submission3",  "context3", "Test model 2","Model used for testing. Made using TextUML",XMIVersionHandler.ECLIPSE_20110701).loadAll()
//    DbLoader("src/test/resources/models/UML_UML241.xmi", "MOD004", "submission4",  "context4", "UML","Model of UML model",XMIVersionHandler.EA_XMI241).loadAll()
//    DbLoader("src/test/resources/models/LSDAM2_2_3_UML241.xmi", "MOD005", "submission5",  "context5", "LS DAM","Lifescience Domain Analysis Model",XMIVersionHandler.EA_XMI241).loadAll()
//    DbLoader("src/test/resources/models/11179-3_UML241.xmi", "MOD006", "submission6", "context6", "ISO 11179 3ed","ISO 11179-3ed spesification", XMIVersionHandler.EA_XMI241).loadAll()
//    DbLoader("src/test/resources/models/caCORE3_UML241.xmi", "MOD008", "submission8",  "context8", "caCORE 3.0","caCORE - NCI",XMIVersionHandler.EA_XMI241).loadAll()
//
//      DbLoader("src/test/resources/models/FuGE-v1-profile.mdxml", "MOD010", "submission10",  "context10", "FuGE v1","UMCG",XMIVersionHandler.MD_XMI21).loadAll()
    //    has nested classes:
    //    DbLoader("src/test/resources/models/caDSR4_UML241.xmi", "MOD007:", "submission7", XMIVersionHandler.EA_XMI241).loadAll()

  }

}