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

class ModelRegistryExporter {

}

object ModelRegistryExporter extends LazyLogging {

  val SEP = "\t"
  private def P(x: String): String = x.replaceAll("\\s+", " ")
  private def M(x: String) : String = if ( x == "*" ) "-1" else x //unlimited multiplicity
  
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
   * Role names are generated if they do not exist
   */
  def exportPackages(contextId: String, submissionID: String, submissionIDSource: String,
    authorityFile: String, packageFile: String, entityFile: String, attributeFile: String, model: UMLModel, prefix: String = "") = {

    //val authOut = openFile(authorityFile)
    val pkgOut = openFile(packageFile)
    val elemOut = openFile(entityFile)
    val attribOut = openFile(attributeFile)

    //print(authOut, "identifier","source_identifier", "name")
    //print(authOut, authorityId, authorityId, submissionIDSource)
    //authOut.close()

    //todo: remove possible duplicates in sublcasses. This may happen if the relationship is defined more than once...
    print(pkgOut, "identifier", "sourceidentifier", "sourcecontext_identifier", "name", "qName","type_identifier", "parentpackage_identifier", "description")
    print(pkgOut, prefix + submissionID, prefix + submissionIDSource, contextId,"name of model here", "qualifiednamexyz","model", "","testing...")
    
    print(elemOut, "identifier", "sourceidentifier", "sourcecontext_identifier","name", "qName","type_identifier", "typeQualifier_identifier", "packageclass_identifier", "parententity_identifier", "description")
   
    print(attribOut, "identifier", "sourceidentifier", "sourcecontext_identifier","name","qName", "entity_identifier", "classifier_identifier", "type_identifier",
      "associationentity_identifier", "navigable", "lowerBound", "upperBound", "aggregation", "description")
    
    val r = model.createResolver
    val UNKNOWN = "unnamed"
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
        print(pkgOut, prefix + p.id, p.id, contextId,p.name.get, p.qualifiedName.getOrElse(UNKNOWN),"package", prefix + parentPkgId, r.commentsAsText(p.id))

        p.entities.filter(e => !e.isInstanceOf[XAssociation]).foreach {
          e =>
            if (e.name.isDefined) {
              val eType = e.entityType
              val eTypeQualifier = if (e.isAbstract.getOrElse(false)) "abstract" else "concrete";
              //r.subClassesAsCsvString(prefix, e.id)
              val parents = e.parentIds
            		  .filter(id => if ( r.resolve(id).isDefined) { true }  else { logger.warn("Parent "+id+" is defined elsewhere. It is currently let out from the export ") ; false })
            		  .map( id => prefix+id ).mkString(",") 
              
              /* PRINT */  
              print(elemOut, prefix + e.id, e.id, contextId, e.name.get, e.qualifiedName.getOrElse(UNKNOWN), eType, eTypeQualifier, prefix + p.id, parents, r.commentsAsText(e.id))

              // loop over attributes. Navigable associations are also attributes in XMI which have also reference to association class
              e.attributes.foreach {
                a =>
                  if (a.typeIdRef.isDefined) {
                    val typeId = r.resolve(a.typeIdRef.get)
                    if (typeId.isDefined) {
                      val attOrAsso = if (a.association.isDefined) ("associationEnd", prefix + a.association.get) else ("attribute", "") //todo: study is it mref.. Add also otherEnd for associations
                      if (attOrAsso._1 != "associationEnd" && !a.name.isDefined) {
                        logger.warn("Attribute " + a.id + " do not have name. This is ok for association ends but not for attributes") //todo: see below 
                      }
                      
                      /* PRINT */  
                      print(attribOut, (prefix + a.id), a.id, contextId, a.name.getOrElse(UNKNOWN), a.qualifiedName.getOrElse(UNKNOWN),(prefix + e.id), (prefix + a.typeIdRef.get),
                        attOrAsso._1, attOrAsso._2, a.isNavigable.getOrElse(false).toString, a.lowerValue.getOrElse(""), M(a.upperValue.getOrElse("")), "", r.allCommentsAsText(a.id))
                        
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
            //Name can be missing. todo: handle the cases...  
            
            /* PRINT */
            print(attribOut, prefix + d.id, d.id,contextId, d.name.getOrElse(UNKNOWN), d.qualifiedName.getOrElse(UNKNOWN),prefix + d.sourceId, prefix + d.targetId,
              "dependency", "", "false", "", "", "", r.allCommentsAsText(d.id))
              
        }

        /*
         * Handle non navigable association ends. I.e. ends which are owned nyt the association 
         */
        p.associations.foreach {
          a =>
            // loop over non-navigable associations (ownedEnd elements) ... should these be included ?
            if (a.memberEndIdRefs.size == 2) {
              
              /* PRINT */
              print(elemOut, prefix + a.id, a.id, contextId, a.name.getOrElse(UNKNOWN),a.qualifiedName.getOrElse(UNKNOWN), "association", "concrete", prefix + p.id, r.subClassesAsCsvString(prefix, a.id), r.allCommentsAsText(a.id))

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
                  //todo: should the end belong to association only i.e. entityclass_identifier should be a.id
                  //now association owned entities are made owned by the UML class (opposite end)
                  
                  /* PRINT */
                  print(attribOut, prefix + oe.id, oe.id, contextId, roleName, UNKNOWN, prefix + otherEndTypeIeThisEntity.get.id, prefix + typeEntity.get.id,
                    "associationEnd", prefix + a.id, oe.isNavigable.getOrElse(false).toString(), oe.lowerValue.getOrElse(""),
                    M(oe.upperValue.getOrElse("")), oe.aggregation.getOrElse(""), r.allCommentsAsText(oe.id))
                    

                //print(attribOut, oe.id, oe.id, roleName, "false", "OWNER_ENTITY="+otherEndTypeIeThisEntity.get.name.getOrElse("UNK1"), "TYPE="+typeEntity.get.name.getOrElse("UNK"), 
                //"associationEnd",a.id,"false", oe.lowerValue.getOrElse("XXXX"), oe.upperValue.getOrElse("YYYY"),r.commentsAsText(oe.id))

              }
            } else {

              logger.warn("Can only handle binary relationships. Association: " + a.id + ".  Number of members: " + a.memberEndIdRefs.size)
            }
        }

    }

    pkgOut.close()
    elemOut.close()
    attribOut.close()

  }

  def test2 = {

    implicit val vsHandler = XMIVersionHandler.ECLIPSE_20110701;
    val file = "src/test/resources/models/model_reg.uml"
    val prefix = "8ac5ae19c779:"
    val submissionID = "MODELREG"
    val submissionIDSource = "MODELREG"
     val contextId = "con0002";

    val outSrc = openFile("SourceContext.tsv")
    //identifier , name, steward, submission
    print( outSrc,"identifier","name","stewardship_identifier","submission_identifier")
    print(outSrc,contextId,"MODELREG","ste0002","sub0002")
    outSrc.close()
    
    val outStw = openFile("StewardshipRecord.tsv")
    //identifier, name, contact, organization
    print(outStw,"identifier","name","contact_identifier","organization_identifier")
    print(outStw,"ste0002","Stewardship name","con0002","org0002")
    outStw.close()
    
    val outSub = openFile("SubmissionRecord.tsv")
    //identifier, name, contact, organization
    print(outSub,"identifier","name","contact_identifier","organization_identifier")
    print(outSub,"sub0002","Submission","con0002","org0002")
    outSub.close()

    val outCon = openFile("Contact.tsv")
    print(outCon,"identifier","name","address","country","email","phone")
    print(outCon,"con0002","Matti Meikäläinen","Mansku XYZ, Helsinki","Finland","xxx@xxx.com","050360XXXX")
    outCon.close()

    val outOrg = openFile("Organization.tsv")
    print(outOrg,"identifier","name","address","country","email")
    print(outOrg,"org0002","XYZ","Insitute XYZ","Netherlands","xxyy@xxx")
    outOrg.close()
  

      
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi)(vsHandler)
    val res = model.createResolver
    (model :: model.allSubPackages).foreach {
      p =>
        p.entities.foreach {
          e =>
            println(e.name.getOrElse("UNK") + " " + e.realizations.length + " " + e.generalizations.length)
        }
    }
    res.subClassMap.foreach {
      s =>
        println(s._1 + " " + s._2)
    }
    exportPackages(contextId,submissionID, submissionIDSource, "PackageClass.tsv", "PackageClass.tsv", "EntityClass.tsv", "AttributeClass.tsv", model, prefix)

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
    print( outSrc,"identifier","name","stewardship_identifier","submission_identifier")
    print(outSrc,contextId,"Test Model","ste0001","sub0001")
    outSrc.close()
    
    val outStw = openFile("StewardshipRecord.tsv")
    //identifier, name, contact, organization
    print(outStw,"identifier","name","contact_identifier","organization_identifier")
    print(outStw,"ste0001","Stewardship..","con0001","org0001")
    outStw.close()
    
    val outSub = openFile("SubmissionRecord.tsv")
    //identifier, name, contact, organization
    print(outSub,"identifier","name","contact_identifier","organization_identifier")
    print(outSub,"sub0001","Submssion...","con0001","org0001")
    outSub.close()

    val outCon = openFile("Contact.tsv")
    print(outCon,"identifier","name","address","country","email","phone")
    print(outCon,"con0001","Juha M","Huvilatie XYZ, Klaukkala","Finland","jmuilu@gmail.com","050360XXXX")
    outCon.close()

    val outOrg = openFile("Organization.tsv")
    print(outOrg,"identifier","name","address","country","email")
    print(outOrg,"org0001","UMCG","Groningen","Netherlands","xxyy@xxx")
    outOrg.close()

    
    exportPackages(contextId, submissionID, submissionIDSource, "PackageClass.tsv", "PackageClass.tsv", "EntityClass.tsv", "AttributeClass.tsv", model, prefix)
  }
  
    def test3 = {
    
    implicit val vsHandler = XMIVersionHandler.EA_XMI241;
    val file = "src/test/resources/models/PrimitiveTypes.xmi"
    val prefix = ""
    val submissionID = "UML_PRIMITIVES"
    val submissionIDSource = "UML_PRIMITIVES"
    val contextId = "con0003"
 
    val xmi = scala.xml.XML.loadFile(file)
    val model = UMLModel(xmi)(vsHandler)

    val outSrc = openFile("SourceContext.tsv")
    //identifier , name, steward, submission
    print( outSrc,"identifier","name","stewardship_identifier","submission_identifier")
    print(outSrc,contextId,"UML Primitives","ste0003","sub0003")
    outSrc.close()
    
    val outStw = openFile("StewardshipRecord.tsv")
    //identifier, name, contact, organization
    print(outStw,"identifier","name","contact_identifier","organization_identifier")
    print(outStw,"ste0003","Stewardship..","con0003","org0003")
    outStw.close()
    
    val outSub = openFile("SubmissionRecord.tsv")
    //identifier, name, contact, organization
    print(outSub,"identifier","name","contact_identifier","organization_identifier")
    print(outSub,"sub0003","Submssion...","con0003","org0003")
    outSub.close()

    val outCon = openFile("Contact.tsv")
    print(outCon,"identifier","name","address","country","email","phone")
    print(outCon,"con0003","Juha M","Huvilatie XYZ, Klaukkala","Finland","jmuilu@gmail.com","050360XXXX")
    outCon.close()

    val outOrg = openFile("Organization.tsv")
    print(outOrg,"identifier","name","address","country","email")
    print(outOrg,"org0003","OMG","Object Management Group","XYZ...","xxyy@xxx")
    outOrg.close()

    
    exportPackages(contextId, submissionID, submissionIDSource, "PackageClass.tsv", "PackageClass.tsv", "EntityClass.tsv", "AttributeClass.tsv", model, prefix)
  }
  def main(args: Array[String]) {

    test1

    //val file = "src/test/resources/models/LSDAM2_2_3_UML241.xmi"
    //val file = "src/test/resources/models/textuml_model.uml"
    //val file = "src/test/resources/models/BRIDG_UML241.xmi"q
    //val file = "src/test/resources/models/UML.xmi"

  }
}