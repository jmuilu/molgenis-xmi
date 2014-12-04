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

class EmxInsertHandler(val fileName: String, resolver: IDResolver) extends InsertHandler with LazyLogging {
  import Tag._

  val tagfile = EmxTags("./src/test/resources/emx_tags.conf")
  val workbook = new Workbook(new XSSFWorkbook());
  val packages = workbook.createSheet("packages")
  val entities = workbook.createSheet("entities")
  val attributes = workbook.createSheet("attributes")
  val tags = workbook.createSheet("tags")
  val tagMap = new HashMap[String, Tag]();

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
  //insertTags()

  def toQNamesStr(ids: List[String]) = ids.map { x => toQname(x) }.mkString(",")

  def toQname(id: String): String = {
    if (id == "") return ""

    val e = resolver.resolve(resolver.toSourceIdentifier(id))
    if (!e.isDefined) {
      logger.warn("ID " + id + " not resolved")
      return id
    }
    val qn = e.get.qualifiedName
    if ( qn.isDefined) {
      return qn.get
    } else {
       "NONAME:"+id 
    }
  }

  //  def insertTags() = {    
  //    val t = tagfile.tags
  //    t.foreach {
  //      t=>
  //      val v = t._2  
  //      tags.addRow(t._1,v.getString("object.label"),v.getString("relation.iri"),v.getString("codesystem"),v.getString("relation.label"))
  //    }
  //    
  //  }
  def insertPackage(identifier: String, name: String, sourceIdentifier: String, description: Option[String],
    parentPackageIdentifier: Option[String], namespaceIdentifier: String) = {
    val parent = parentPackageIdentifier.getOrElse("")
    val desc = description.getOrElse("")
    packages.addRow(toQname(identifier), desc, parent)
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
    val t = Tag(c, target, harmonize(label), target)
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

    val e = resolver.resolveClassifierType(sourceIdentifier)
    assert(e.isDefined)
    if (e.get == CLASS || e.get == INTERFACE) {
      val tagIds = tagIdList(GENERALIZATON, generalizations) ++ tagIdList(REALIZATION, realizations)
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
      logger.warn("Cannot handle: " + e.get)
    }

  }

  def resolvePrimitiveType(cls: XClassifier): String = {
    if (cls.entityType == PRIMITIVE) {
      return cls.name.getOrElse("UNKNOWN")
    } else {
      return cls.name.getOrElse("UNKNOWN")
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
      if (e.get == CLASS || e.get == INTERFACE) {

        val desc = description.getOrElse("")
        val _classifier = resolver.resolve(resolver.toSourceIdentifier(classifierIdentifier))
        if (! _classifier.isDefined ) {
          logger.error("Classifier "+ resolver.toSourceIdentifier(classifierIdentifier)+" not found. Check "+sourceIdentifier)
        } else {
          val classifier = _classifier.get.asInstanceOf[XClassifier]
          val (dataType, refEnt, literals) = if (associationIdentifier.isDefined) {
            val refName = toQname(classifierIdentifier)
            if (!upperBound.isDefined || upperBound.get.equals("1") || upperBound.get.equals("0")) {
              ("xref", refName, "")
            } else {
              logger.debug("Multip: " + lowerBound + " -" + upperBound)
              ("mref", refName, "")
            }
          } else {
            if (classifier.entityType == ENUMERATION) {
              ("enum", "", classifier.asInstanceOf[XEnumeration].literals.map { l => l.name.get }.mkString(","))
            } else {
              (classifierName.getOrElse("UNKNOWN"), "", "");
            }
          }
          if (dataType == "enum") {
            val enum = classifier.asInstanceOf[XEnumeration]
            enum.literals.map { l => l.name.get }
          }
          val nillable = (if (upperBound.isDefined && upperBound.get.equals("0")) true else false).toString().toUpperCase();
          val tags = (upperBound.map { p => tagLiteral(MAXVAL, p) } ++ lowerBound.map { p => tagLiteral(MINVAL, p) }).toList.mkString(",")
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
  def apply(c: Config, objIri: String, objLabel: String, range: String) = new Tag(c.getString("iri"),
    c.getString("label"), objIri, objLabel, range)
}

class EmxTags(fileName: String) {
  import Tag._
  val file = new File(fileName)
  val cfg = ConfigFactory.parseFile(file).resolve
  val relations = List(MINVAL, MAXVAL, REALIZATION, GENERALIZATON)
  val relation = Map(relations.map(r => (r, cfg.getConfig("tags.relation." + r))).toList: _*)
  def emxid(rel: String, target: Any) = relation.get(rel).get.getString("id") + ":" + target.toString()

}

object EmxTags {

  def apply(fileName: String) = new EmxTags(fileName)

  def main(args: Array[String]): Unit = {

    val tgs = EmxTags("./src/test/resources/emx_tags.conf")
  }
}