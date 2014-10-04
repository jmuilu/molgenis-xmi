package xmitools.db.sql

import xmitools.model.XPackage
import xmitools.model.XClassifier
import xmitools.model.UMLModel
import scala.slick.driver.MySQLDriver.simple._
import xmitools.model.XMIVersionHandler
import xmitools.model.XProperty
import java.beans.Introspector
import sun.security.pkcs11.Secmod.DbMode

class DbLoader(model: UMLModel, prefix: String, rootId: String) extends ModelLoader(model, prefix, rootId) {
  val DB = new ModelDB

  def insert(pkg: XPackage, rootId: String) = {
    DB.DB.withSession {
      implicit session =>
        /* 
       * name, identifier, sourceIdentifier, description, packageIdentifier 
       */
        DB.packages += (getOpt(pkg.name), pfix(pkg.id), pkg.id, comments(pkg), toOpt(pfix(rootId)))
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
        DB.entities += (pfix(e.id), getOpt(e.name), e.id, comments(e), eType, eTypeQualifier, pfix(pkgId))

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
        val typeEntity = resolver.resolve(p.typeIdRef.get)
        if (typeEntity.isDefined) {
          val (typeCode, assoIdentifier) = if (p.association.isDefined) ("associationEnd", prefix + p.association.get) else ("attribute", "")
          assert(p.parent.isDefined, "Property do not belong to a entinty. Property: " + p.id)
          val entityIdentifier = p.parent.get.id
          DB.attributes += (pfix(p.id), getOpt(p.name), p.id, comments(p), typeCode, "",
            entityIdentifier, typeEntity.get.id, toOpt(assoIdentifier), p.lowerValue, p.upperValue, true)
        } else {
          logger.warn("Type of property " + p.id + " not resolved")
        }

    }
  }

  def insertPropertyOwnByAssociationEntity(p: XProperty) = {
    DB.DB.withSession {
      implicit session =>
        /*
       * identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier, typeIdentifier, associationIdentifier, lowerBound, upperBound,navigable
       */
        val typeEntity = resolver.resolve(p.typeIdRef.get)
        assert(typeEntity.isDefined && typeEntity.get.name.isDefined, "Check type of association end " + p.id)
        if (typeEntity.isDefined) {
          val name = deriveRoleNameForAssociationEnd(p, typeEntity.get)
          val (typeCode, assoIdentifier) = if (p.association.isDefined) ("associationEnd", prefix + p.association.get) else ("attribute", "")
          assert(!assoIdentifier.isEmpty())
          assert(p.parent.isDefined, "Property do not belong to a entinty. Property: " + p.id)
          val associationEntityIdentifier = p.parent.get.id
          assert(assoIdentifier == associationEntityIdentifier)
          val otherEnd = resolver.resolveOtherEndOfBinaryAssociation(associationEntityIdentifier, p.id)
          assert(otherEnd.isDefined)
          assert(otherEnd.get.typeIdRef.isDefined, "Type is missing in other end. Association end is " + otherEnd.get.id)
          val otherEndTypeIeThisEntity = resolver.resolve(otherEnd.get.typeIdRef.get)
          //todo: should the end belong to association only i.e. entityclass_identifier should be a.id
          //now association owned entities are made owned by the UML class (opposite end)

          DB.attributes += (pfix(p.id), name, p.id, comments(p), "associationEnd", "",
          pfix(otherEndTypeIeThisEntity.get.id), pfix(typeEntity.get.id), toOpt(pfix(associationEntityIdentifier)), p.lowerValue, p.upperValue, p.isNavigable.getOrElse(false))
        } else {
          logger.warn("Type of property " + p.id + " not resolved")
        }

    }
  }

}

object DbLoader {

  def apply(file: String, prefix: String, submissionId: String, submissionIdSource: String, contextId: String) = {
    implicit val vsHandler = XMIVersionHandler.EA_XMI241;
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi)(vsHandler)
    new DbLoader(model, submissionIdSource, submissionId)
  }

  def main(args: Array[String]): Unit = {

    val m = new ModelDB
    m.recreate
    
    val file = "src/test/resources/models/TEST_MODEL_UML241.xmi"
    val loader = DbLoader(file, "MOD:", "xyz001:", "xyz000", "context")
    loader.loadAll()
  }

}