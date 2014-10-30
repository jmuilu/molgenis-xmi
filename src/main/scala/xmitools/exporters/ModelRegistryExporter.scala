package xmitools.exporters

import xmitools.model.UMLModel
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.slf4j.LazyLogging
import xmitools.model.XProperty
import java.beans.Introspector
import xmitools.model.XInterface
import xmitools.model.XDataType
import xmitools.model.XAssociation
import xmitools.model.XMIVersionHandler
import xmitools.model.Ns
import xmitools.model.XClassifier
import xmitools.model.XEnumeration
import org.w3c.dom.TypeInfo

class ModelRegistryExporter {

}
/*
 * TODO: Took old code here and refactor
 * 
 */
object ModelRegistryExporter extends DataCleaner {

  val SEP = "\t"

  def makeRow(fields: Seq[String]) = fields.map(f => P(f)).toList.mkString(SEP)

  var map = new HashMap[String, String]

  def openFile(name: String) = new PrintWriter(new File(name))
  def print(out: PrintWriter, fields: String*) = {
    //first field is always id field
    assert(fields.size > 0)
    if (map.isDefinedAt(fields(0))) {
      logger.warn("not inserting the model element. It is already defined in another package. Element: " + fields.mkString(", "))
    } else {
      out.println(makeRow(fields))
    }
  }

  /*
   * Note:
   * Do not export super classes, which do not exist in model e.g. http://www.omg.org/spec/UML/20110701/UML.xmi#int for primitive types
   * 
   * Role names are generatef dor assications if they do not exist
   */
  def exportPackages(administeredInfoId: String, submissionID: String, submissionIDSource: String, packageFile: String, entityFile: String, attributeFile: String, model: UMLModel, prefix: String = "") = {

    //val authOut = openFile(authorityFile)
    val O_PKG = openFile(packageFile)
    val O_ELEM = openFile(entityFile)
    val O_ATTR = openFile(attributeFile)

    print(O_PKG, "identifier", "name", "description", "administeredInfo_identifier", "subPackages_identifier", "tags_identifier")
    print(O_ELEM, "identifier", "name", "description", "fullName", "abstract", "package_identifier", "extends_identifier", "tags_identifier")
    print(O_ATTR, "identifier", "name", "description", "entity_identifier", "dataType", "refEntity_identifier", "nillable", "auto", "enum_identifiers", "tags_identifier")

    val r = model.createResolver
    val UNKNOWN = "unnamed"

    print(O_PKG, prefix + submissionID, "NAME OF THE MODEL", "DESCRIPTION", administeredInfoId, model.subPackages.map(p => prefix + p.id).mkString(","))
    (model :: model.allSubPackages).foreach {
      p =>

        val parentPkgId =
          if (p.parent.isDefined) {
            if (p.parent.get.id == p.id) {
              submissionID
            } else {
              p.parent.get.id
            }
          } else {
            submissionID
          };

        /* PRINT */
        print(O_PKG, prefix + p.id, p.name.get, r.commentsAsText(p.id), administeredInfoId, p.subPackages.map(p => prefix + p.id).mkString(","))

        p.entities.filter(e => e.entityType == "class" || e.entityType == "interface").foreach {
          // we skip following entities: association, primitive (primitive type and datatype)), enumeration and artifact
          e =>
            if (e.name.isDefined) {

              val isAbstract = if (e.entityType == "interface" || (e.isAbstract.isDefined && e.isAbstract.get)) "true" else "false"

              val parents = e.parentIds
                .filter(id => if (r.resolve(id).isDefined) { true } else { logger.warn("Parent " + id + " is defined elsewhere. It is currently let out from the export "); false })
                .map(id => prefix + id).mkString(",")

              /* PRINT */
              print(O_ELEM, prefix + e.id, e.name.get, r.commentsAsText(e.id), e.qualifiedName.getOrElse(UNKNOWN), isAbstract, prefix + p.id, parents, "")

              // loop over attributes and association ends
              e.attributes.foreach {
                a =>
                  if (a.typeIdRef.isDefined) {
                    // type of attribute id defined by an entity (primitive, class interface...)
                    val _type = r.resolve(a.typeIdRef.get)
                    if (_type.isDefined) {
                      assert(_type.get.isInstanceOf[XClassifier], "Something went wrong. Entity " + _type.get.id + " must be a classifier. Property of " + e.id)
                      val typeEntity = _type.get.asInstanceOf[XClassifier]
                      val enumLiterals = if (typeEntity.isInstanceOf[XEnumeration]) {
                        val res = typeEntity.asInstanceOf[XEnumeration].literals.map(
                          l => if (l.name.isDefined) l.name.get else { logger.error("Literal " + l.id + " do not have name. Name set to " + UNKNOWN); UNKNOWN })
                        res.mkString(",")
                      } else {
                        ""
                      }

                      val primitiveType = if (typeEntity.entityType == "primitive") {
                        if (typeEntity.name.isDefined) {
                          typeEntity.name.get //todo: resolve type
                        } else {
                          logger.error("Type " + typeEntity.id + " do not have name")
                          UNKNOWN
                        }
                      } else {
                        if (a.association.isDefined) {
                          if (isMoreThanOne(a.upperValue)) {
                            "mref"
                          } else {
                            "xref"
                          }
                        } else {
                          logger.error("Type " + typeEntity.id + " is unknown ")
                          UNKNOWN
                        }
                      }

                      val isNillable = if (isZero(a.upperValue)) "true" else "false"

                      val isAuto = "false"

                      val refEntity = if (primitiveType == "xref" || primitiveType == "mref") {
                        prefix + typeEntity.id
                      } else {
                        ""
                      }

                      /* PRINT */
                      //"identifier", "name", "description", "entity_identifier", "dataType", "refEntity_identifier", "nillable", "auto", "enum_identifiers", "tags_identifier"
                      print(O_ATTR, (prefix + a.id), a.name.getOrElse(UNKNOWN), r.commentsAsText(a.id), e.id, primitiveType, refEntity, isNillable, isAuto, enumLiterals, "")

                    } else {
                      logger.warn("Attribute's " + a.id + " type do not resolve. Type id = " + a.typeIdRef.get)
                    }
                  } else {
                    logger.warn("Attribute " + a.id + " do not have type")
                  }
              }

            } else {
              logger.warn("Enity " + e.id + " do not have name");
            }
        }

        p.dependencies.foreach {
          d =>
            logger.warn("Dependency not supported. Dependency " + p.id)
        }

        /*
         * Handle non owned associations (association which are owned by Association entity) 
         */
        p.associations.foreach {
          a =>
            // loop over non-navigable associations (ownedEnd elements) ... should these be included ?

            if (a.memberEndIdRefs.size == 2) {

              /* PRINT */

              a.ownedEnds.foreach {
                oe =>
                  assert(oe.typeIdRef.isDefined, "Association ownEnd " + oe.id + " do not have type")

                  val typeEntity = r.resolve(oe.typeIdRef.get)
                  //todo: error in BRIDG java.lang.AssertionError: assertion failed: Check type of association end EAID_src038024_5EDA_44c0_BADD_8F47B74A4011
                  assert(typeEntity.isDefined && typeEntity.get.name.isDefined, "Check type of association end " + oe.id)
                  val roleName =
                    if (!oe.name.isDefined) {
                      if (oe.isNavigable.isDefined && oe.isNavigable.get) {
                        val _name = Introspector.decapitalize(typeEntity.get.name.get)
                        logger.warn("Role name of navigable association end " + oe.id + " is undefined. Attribute (xref) name will be set to " + _name)
                        _name
                      } else {
                        UNKNOWN
                      }
                    } else {
                      oe.name.get
                    }

                  val otherEnd = r.resolveOtherEndOfBinaryAssociation(a.id, oe.id)
                  assert(otherEnd.isDefined)
                  assert(otherEnd.get.typeIdRef.isDefined, "Type is missing in other end. Association end is " + otherEnd.get.id)
                  val otherEndTypeIeThisEntity = r.resolve(otherEnd.get.typeIdRef.get)
                  assert(otherEndTypeIeThisEntity.isDefined, "Check association " + a.id + " other end of " + oe.id + " is not  properly defined");
                  val entityIdentifier = otherEndTypeIeThisEntity.get.id

                  val isNillable = if (isZero(oe.upperValue)) "true" else "false"

                  val primitiveType = if (isMoreThanOne(oe.upperValue)) {
                    "mref"
                  } else {
                    "xref"
                  }

                  /* PRINT */
                  //"identifier", "name", "description", "entity_identifier", "dataType", "refEntity_identifier", "nillable", "auto", "enum_identifiers", "tags_identifier"
                  print(O_ATTR, (prefix + oe.id), roleName, r.commentsAsText(oe.id), prefix + entityIdentifier, primitiveType, prefix + typeEntity.get.id, isNillable, "false", "", "")

              }
            } else {

              logger.warn("Can only handle binary relationships. Association: " + a.id + ".  Number of members: " + a.memberEndIdRefs.size)
            }
        }

    }

    O_PKG.close()
    O_ELEM.close()
    O_ATTR.close()

  }

  def test1 = {

    implicit val vsHandler = XMIVersionHandler.EA_XMI241;

    val file = "src/test/resources/models/TEST_MODEL_UML241.xmi"
    val prefix = "TEST_MODEL:"
    val submissionID = "TEST_MODEL"
    val submissionIDSource = "TEST_MODEL"
    val contextId = "con0001"

    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi)(vsHandler)

    val outSrc = openFile("SourceContext.tsv")
    //identifier , name, steward, submission
    print(outSrc, "identifier", "name", "stewardship_identifier", "submission_identifier")
    print(outSrc, contextId, "Test Model", "ste0001", "sub0001")
    outSrc.close()

    val outStw = openFile("StewardshipRecord.tsv")
    //identifier, name, contact, organization
    print(outStw, "identifier", "name", "contact_identifier", "organization_identifier")
    print(outStw, "ste0001", "Stewardship..", "con0001", "org0001")
    outStw.close()

    val outSub = openFile("SubmissionRecord.tsv")
    //identifier, name, contact, organization
    print(outSub, "identifier", "name", "contact_identifier", "organization_identifier")
    print(outSub, "sub0001", "Submssion...", "con0001", "org0001")
    outSub.close()

    val outCon = openFile("Contact.tsv")
    print(outCon, "identifier", "name", "address", "country", "email", "phone")
    print(outCon, "con0001", "Juha M", "Huvilatie XYZ, Klaukkala", "Finland", "jmuilu@gmail.com", "050360XXXX")
    outCon.close()

    val outOrg = openFile("Organization.tsv")
    print(outOrg, "identifier", "name", "address", "country", "email")
    print(outOrg, "org0001", "UMCG", "Groningen", "Netherlands", "xxyy@xxx")
    outOrg.close()

    exportPackages(contextId, submissionID, submissionIDSource, "PackageMetaData.tsv", "EntityMetaData.tsv", "AttributeMetaData.tsv", model, prefix)
  }

  def testx  = {
   
    implicit val vsHandler = XMIVersionHandler.EA_XMI241;
    val file = "src/test/resources/models/BRIDG_UML241.xmi"
    System.out.println("Load file...");
    val xmi = scala.xml.XML.loadFile(file)
    System.out.flush();
    System.out.println("Loading...");
    val model = UMLModel(xmi)(vsHandler)
    System.out.println("Done...Resolving");
    System.out.flush();
    val res = model.createResolver
    val x = res.resolve("x")
    System.out.println("Done...");
    System.out.println(model.allPackages.map(p=>p.name.get).mkString(",")) ;
  }

  def main(args: Array[String]) {

    testx

    //val file = "src/test/resources/models/LSDAM2_2_3_UML241.xmi"
    //val file = "src/test/resources/models/textuml_model.uml"
    //val file = "src/test/resources/models/BRIDG_UML241.xmi"q
    //val file = "src/test/resources/models/UML.xmi"

  }
}