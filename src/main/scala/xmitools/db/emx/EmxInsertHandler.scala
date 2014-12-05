package xmitools.db.emx

import java.io.FileInputStream
import java.io.File
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.xssf.usermodel.XSSFSheet
import xmitools.db.sql.InsertHandler
import xmitools.model.IDResolver
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.io.FileOutputStream
import xmitools.model.XClassifier
import xmitools.model.entityTypes._
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._
import com.typesafe.config.ConfigParseOptions
import scala.collection.mutable.HashMap
import com.typesafe.config.Config
import xmitools.model.XEnumeration
import xmitools.model.XNode

class Workbook(workbook: XSSFWorkbook) {
  def createSheet(name: String) = new Sheet(workbook.createSheet(name))

  def write(fileName: String) = {
    val fileOut = new FileOutputStream(fileName);
    workbook.write(fileOut)
    fileOut.close()
  }
}

class Sheet(xsheet: XSSFSheet) {
  var currentRow = 0;
  val sheet = xsheet
  def addRow(values: String*) = {
    val row = sheet.createRow(currentRow);
    var cellId = 0
    values.foreach {
      v =>
        val cell = row.createCell(cellId)
        cell.setCellValue(v)
        cellId = cellId + 1
    }
    currentRow = currentRow + 1;
  }

}

class EmxInsertHandler(val fileName: String, resolver: IDResolver, tagfile: EmxTags, modelName: String) extends InsertHandler with LazyLogging {
  import Tag._

  val workbook = new Workbook(new XSSFWorkbook());
  val packages = workbook.createSheet("packages")
  val entities = workbook.createSheet("entities")
  val attributes = workbook.createSheet("attributes")
  val tags = workbook.createSheet("tags")
  val tagMap = new HashMap[String, Tag]();

  val entityNameMap = new HashMap[String, Int]()
  val entityIdMap = new HashMap[String, String]()

  packages.addRow("name", "description", "parent", "tags");
  entities.addRow("name", "package", "description", "abstract", "label", "extends", "tags")
  attributes.addRow("name", "entity", "dataType", "refEntity", "nillable", "enumOptions",
    "readOnly", "aggregateable", "unique", "description", "tags")

  def createTagSheet = {
    tags.addRow("identifier", "objectIRI", "objectLabel", "relationLabel", "codeSystem", "relationIri")
    tagMap.foreach {
      t =>
        tags.addRow(t._1, t._2.objectIri, t._2.objectLabel, t._2.relationLabel, "uml", t._2.relationIri)
    }

  }

  def hackTheName(name: String): String = {
    var _name = if (name.length() > 64) {
      name.substring(0, 64);
    } else {
      name
    }
    //    if ( entityNameMap.isDefinedAt( _name)) {
    //      val n = entityNameMap.get(_name).get
    //      _name = _name + (n + 1)
    //      entityNameMap.put(_name , n + 1)
    //    }
    //  need to pre-process the names    
    return _name;
  }

  def toEMXName(name: String): String = {
    val fixed = name.replaceAll("[^A-Za-z0-9_]", "").replaceAll("^[0-9]", "X");
    return hackTheName(fixed)
  }

  def toQNamesStr(ids: List[String]) = ids.map { x => toQname(x) }.mkString(",")

  /** EMX uses names not identifiers/GUIDs. Need to hack the names in */
  def toQname(id: String): String = {
    if (id == "") return ""

    val e = resolver.resolve(resolver.toSourceIdentifier(id))
    if (!e.isDefined) {
      logger.warn("ID " + id + " not resolved")
      return id
    }
    //val qn = e.get.qualifiedName
    val qn = e.get.name
    toEMXName(if (qn.isDefined) {
      modelName + "_" + qn.get
    } else {
      logger.warn("Entity do not have name. Id used instead" + id)
      id
    })
  }

  def insertPackage(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    parentPackageIdentifier: Option[String], namespaceIdentifier: String) = {
    val parent = parentPackageIdentifier.getOrElse("")
    val desc = description.getOrElse("")
    val tags = List(tagLiteral(SOURCEID, sourceIdentifier), tagLiteral(SOURCENAME, name))
    packages.addRow(toQname(identifier), desc, toQname(parent), tags.mkString(","))
  }

  def extractLabel(label: String): String = {
    val r = label.split("\\.")
    if (r.size > 1) {
      return r.last
    } else {
      return label
    }
  }

  def fixSpecial(chr: String): String = {
    chr match {
      case "*" => "many"
      case _ => chr
    }
  }

  def harmonize(chr: String): String = {
    chr match {
      case "-1" => "*"
      case _ => chr
    }
  }

  def tagLiteral(relation: String, label: String): String = {
    val c = tagfile.relation(relation)
    val target = c.getString("range")
    val i = tagfile.emxid(relation, fixSpecial(harmonize(label)))
    val t = Tag(c, "", harmonize(label), target)
    tagMap.put(i, t)
    return i
  }

  def tagIdList(relation: String, ids: List[String]): List[String] = {

    val c = tagfile.relation(relation)
    val tagIds = ids.map {
      p =>
        val target = toQname(p)
        val i = tagfile.emxid(relation, target)
        val t = Tag(c, target, extractLabel(target), c.getString("range"))
        tagMap.put(i, t)
        i
    }
    return tagIds
  }

  def insertEntity(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    typeCv: String, typeQualifierCv: String, packageIdentifier: String, instanceOfIdentifier: Option[String],
    generalizations: List[String], realizations: List[String]) = {

    if (!entityIdMap.isDefinedAt(identifier)) {
      entityIdMap.put(identifier, "DONE");
      val e = resolver.resolveClassifierType(sourceIdentifier)
      assert(e.isDefined)
      if (e.get == CLASS || e.get == INTERFACE) {

        val tagIds = tagIdList(GENERALIZATON, generalizations) ++
          tagIdList(REALIZATION, realizations) ++
          List(tagLiteral(SOURCEID, sourceIdentifier), tagLiteral(SOURCENAME, name))

        val desc = description.getOrElse("")
        val abs = if (typeQualifierCv == "abstract") "true" else "false";
        val ext = if (generalizations.length > 0) {
          if (generalizations.size > 1) {
            logger.warn("Cannot handle multiple inheritance properly");
          }
          generalizations(0)
        } else {
          ""
        }
        entities.addRow(toQname(identifier), toQname(packageIdentifier), desc, abs, name, toQname(ext), tagIds.mkString(","))
      } else {
        if (e.get == PRIMITIVE) {
          logger.warn("Primitives handled differenlty" + e.get)
        } else {
          if (e.get != ASSOCIATION) logger.warn("Following entities are ignored...: " + e.get)
        }
      }
    }
  }

  def resolvePrimitiveType(cls: XClassifier): String = {
    if (cls.entityType == PRIMITIVE) {
      return cls.name.getOrElse("UNKNOWN")
    } else {
      return cls.name.getOrElse("UNKNOWN")
    }
  }

  def resolveEXMDataType(cls: XNode): Option[String] = {

    if (!cls.name.isDefined) return None
    val name = cls.name.get
    if (tagfile.resolveType(name).isDefined) {
      return tagfile.resolveType(name)
    } else {
      None
    }
  }

  def insertAttribute(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    typeCv: String, typeQualifierCv: String, entityIdentifier: String,
    classifierIdentifier: String, classifierName: Option[String],
    associationIdentifier: Option[String], lowerBound: Option[String],
    upperBound: Option[String], navigable: Boolean,
    aggregation: Option[String]) = {

    val e = resolver.resolveClassifierType(resolver.toSourceIdentifier(entityIdentifier))
    assert(e.isDefined, "Entity " + entityIdentifier + " is not classifier")
    if (e.get != ENUMERATION) {
      /** enums are implemented as EMX datatypes*/
      if (e.get == CLASS || e.get == INTERFACE) {

        val desc = description.getOrElse("")
        val _classifier = resolver.resolve(resolver.toSourceIdentifier(classifierIdentifier))
        if (!_classifier.isDefined) {
          logger.error("Classifier " + resolver.toSourceIdentifier(classifierIdentifier) + " not found. Check " + sourceIdentifier)
        } else {
          
          val classifier = _classifier.get.asInstanceOf[XClassifier]
          val (dataType, refEnt, literals, isDatatypeConvertedToEMXEntity) = if (associationIdentifier.isDefined) {
            val refName = toQname(classifierIdentifier)
            if (!upperBound.isDefined || upperBound.get.equals("1") || upperBound.get.equals("0")) {
              ("xref", refName, "", None)
            } else {
              logger.debug("Multip: " + lowerBound + " -" + upperBound)
              ("mref", refName, "", false)
            }
          } else {
            if (classifier.entityType == ENUMERATION) {
              ("enum", "", classifier.asInstanceOf[XEnumeration].literals.map { l => l.name.get }.mkString(","), false)
            } else {
              //this is  getting rather hacky.. 
              val cls = resolveEXMDataType(classifier)
              val (datatype, isDatatypeConvertedToEMXEntity)  = if (cls.isDefined) {
                /** ok we were able to resolve the EMX data type*/
                (cls.get, false)
              } else {
                /** datatype not resolved. Need to store it as EMX entity and make xref*/
                val eType = classifier.entityType //class,interface,association, primitive...
                val eTypeQualifier = if (classifier.isAbstract.getOrElse(false)) "abstract" else "concrete";
                assert(classifier.parent.isDefined, "Entity do not belong to a package. Entity: " + classifier.id)
                val pkgId = classifier.parent.get.id
                def toOpt(str: String): Option[String] = {
                  if (str == null || str == "") {
                    None
                  } else {
                    Some(str)
                  }
                }
                def comments(e: XNode): Option[String] = {
                  val comm = resolver.commentsAsText(e.id)
                  return toOpt(comm)
                }
                val gList = classifier.generalizationIds.map(x => resolver.pfix(x))
                val rList = classifier.realizationIds.map(x => resolver.pfix(x))
                insertEntity(resolver.pfix(classifier.id), classifier.name.get, classifier.id, comments(classifier), eType, eTypeQualifier, resolver.pfix(pkgId), None, gList, rList)
                ("xref", true)
              }
              if ( isDatatypeConvertedToEMXEntity) {
                val refName = toQname(classifierIdentifier)
                ("xref",refName,"",true)                
              } else {
            	  (datatype, "", "", false);
              }
            }
          }
          if (dataType == "enum") {
            val enum = classifier.asInstanceOf[XEnumeration]
            enum.literals.map { l => l.name.get }
          }

          val nillable = (if (upperBound.isDefined && upperBound.get.equals("0")) true else false).toString().toUpperCase();

          val tags = (
            upperBound.map { p => tagLiteral(MAXVAL, p) } ++
            lowerBound.map { p => tagLiteral(MINVAL, p) } ++
            List(tagLiteral(SOURCEID, sourceIdentifier), tagLiteral(SOURCENAME, name))).toList.mkString(",")

          //logger.warn("Navi: " + dataType + " " + navigable.toString())
          val qname = toQname(entityIdentifier) + "." + name
          attributes.addRow(qname, toQname(entityIdentifier), dataType, refEnt, nillable, literals, "",
            aggregation.getOrElse(""), "", desc, tags)
        }
      }
    }
  }

  def close() {
    createTagSheet
    workbook.write(fileName)
  }

}
class Tag(val relationIri: String, val relationLabel: String,
  val objectIri: String, val objectLabel: String, val range: String)

object Tag {
  val MINVAL = "minval"
  val MAXVAL = "maxval"
  val REALIZATION = "realizationOf"
  val GENERALIZATON = "generalizationOf"
  val SOURCEID = "sourceId"
  val SOURCENAME = "sourceName"
  def apply(c: Config, objIri: String, objLabel: String, range: String) = new Tag(c.getString("iri"),
    c.getString("label"), objIri, objLabel, range)
}

class EmxTags(fileName: String) {
  import Tag._
  val file = new File(fileName)
  val cfg = ConfigFactory.parseFile(file).resolve
  val relations = List(MINVAL, MAXVAL, REALIZATION, GENERALIZATON, SOURCEID, SOURCENAME)
  val relation = Map(relations.map(r => (r, cfg.getConfig("tags.relation." + r))).toList: _*)
  def emxid(rel: String, target: Any) = relation.get(rel).get.getString("id") + ":" + target.toString()
  def types = cfg.getStringList("emx.datatypes")
  def typemap = Map(types.map { t => (t, t); }.toList: _*)
  def resolveType(name: String): Option[String] = {
    return typemap.get("x" + name.toLowerCase())
  }

}

object EmxTags {

  def apply(fileName: String) = new EmxTags(fileName)

  def main(args: Array[String]): Unit = {

    val tgs = EmxTags("./src/test/resources/emx_tags.conf")
  }
}