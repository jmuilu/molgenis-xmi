package xmitools.db.sql;

import xmitools.model.XPackage
import xmitools.model.XClassifier
import xmitools.model.UMLModel
import xmitools.model.XMIVersionHandler
import xmitools.model.XProperty
import xmitools.model.XInstance
import scala.xml.Elem
import xmitools.db.emx.EmxInsertHandler
import com.typesafe.scalalogging.slf4j.LazyLogging
 
class MDRDbInsertHanlder extends InsertHandler with LazyLogging {
	  import scala.slick.driver.MySQLDriver.simple._
	  val DB = new ModelDB

	  def insertPackage(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
	    parentPackageIdentifier: Option[String], namespaceIdentifier: String) = {
	    DB.DB.withSession {
	      implicit session =>
	        DB.packages += (identifier, name, sourceIdentifier, description, parentPackageIdentifier, namespaceIdentifier);
	    }

	  }

	  def insertEntity(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
	    typeCv: String, typeQualifierCv: String, packageIdentifier: String, instanceOfIdentifier: Option[String], generalizations: List[String], realizations: List[String]) = {

      val parentIdentifiers = generalizations ++ realizations // we put these together now
	    DB.DB.withSession {
	      implicit session =>
	        DB.entities += (identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, packageIdentifier, instanceOfIdentifier)
	        parentIdentifiers.foreach { x =>
	          DB.entityExtends += (identifier, x)
	        }
	    }
	  }

	  def insertAttribute(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
	    typeCv: String, typeQualifierCv: String, entityIdentifier: String,
	    classifierIdentifier: String, classifierName: Option[String],
	    associationIdentifier: Option[String], lowerBound: Option[String],
	    upperBound: Option[String], navigable: Boolean,
	    aggregation: Option[String]) = {

	    DB.DB.withSession {
	      implicit session =>
	        DB.attributes += (identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier,
	          classifierIdentifier, associationIdentifier, lowerBound, upperBound, navigable, aggregation);
	    }
	  }
	  
	  def close = {
	    // todo: optional commit here..
	  }

	}
