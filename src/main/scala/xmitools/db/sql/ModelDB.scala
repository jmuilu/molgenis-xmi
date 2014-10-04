package xmitools.db.sql
import scala.slick.driver.MySQLDriver.simple._

class ModelDB {

  val db_user = "molgenis"
  val db_password = "molgenis"
  val db_uri = "jdbc:mysql://localhost/omx"

  val DB = Database.forURL(url = db_uri, user = db_user, password = db_password, driver = "com.mysql.jdbc.Driver")

  class MDRPackage(tag: Tag) extends Table[(String, String, String, Option[String], Option[String])](tag, "MDRPackage") {
    def identifier = column[String]("identifier", O.PrimaryKey)
    def sourceIdentifier = column[String]("sourceIdentifier")
    def name = column[String]("name")
    def description = column[Option[String]]("description")
    def packageIdentifier = column[Option[String]]("packageIdentifier")
    def pkgIdx = index("packagePackageIdx", (packageIdentifier), unique = false)
    def * = (name, identifier, sourceIdentifier, description, packageIdentifier)
  }
  val packages = TableQuery[MDRPackage]

  class MDREntity(tag: Tag) extends Table[(String, String, String, Option[String], String, String, String)](tag, "MDREntity") {
    def identifier = column[String]("identifier", O.PrimaryKey)
    def sourceIdentifier = column[String]("sourceIdentifier")
    def name = column[String]("name")
    def description = column[Option[String]]("description")
    def typeCv = column[String]("typeCv")
    def typeQualifierCv = column[String]("typeQualifierCv")
    def packageIdentifier = column[String]("packageIdentifier")
    //def pkgFK = foreignKey("packageFF", packageIdentifier, packages)(_.identifier)
    def pkgIdx = index("entityPackageIdx", (packageIdentifier), unique = false)
    def * = (identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, packageIdentifier)
  }
  val entities = TableQuery[MDREntity]

  class MDRAttribute(tag: Tag) extends Table[(String, String, String, Option[String], String, String, String, String, Option[String], Option[String], Option[String], Boolean)](tag, "MDRAttribute") {
    def identifier = column[String]("identifier", O.PrimaryKey)
    def sourceIdentifier = column[String]("sourceIdentifier")
    def name = column[String]("name")
    def typeCv = column[String]("typeCv")
    def typeQualifierCv = column[String]("typeQualifierCv")
    def description = column[Option[String]]("description")
    def entityIdentifier = column[String]("entityIdentifier")
    def typeIdentifier = column[String]("typeIdentifier")
    def associationIdentifier = column[Option[String]]("associationIdentifier")
    //def entityFK = foreignKey("attribEntityFK", entityIdentifier, entities)(_.identifier)
    //def typeFK = foreignKey("attribTypeFK", typeIdentifier, entities)(_.identifier)
    def attributeIdx = index("attribEntityIdx", (entityIdentifier), unique = false)
    def typeIdx = index("attribTypeIdx", (typeIdentifier), unique = false)
    def lowerBound = column[Option[String]]("lowerBound")
    def upperBound = column[Option[String]]("upperBound")
    def navigable = column[Boolean]("navigable")
    def * = (identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier, typeIdentifier, associationIdentifier, lowerBound, upperBound, navigable)
  }

  val attributes = TableQuery[MDRAttribute]

  class MDREntityExtend(tag: Tag) extends Table[(String, String)](tag, "MDREntityExtends") {
    def entityIdentifier = column[String]("entityId")
    def extendsIdentifier = column[String]("extendsId")
    def pk = primaryKey("pkEntityExtends", (entityIdentifier, extendsIdentifier))
    def entityExtendIdx = index("entityExtendIdx", (entityIdentifier, extendsIdentifier), unique = true)
    //def entityFK = foreignKey("entityExtendsFK", entityIdentifier, entities)(_.identifier)
    //def extendsFK = foreignKey("extendsExtendsFK", extendsIdentifier, entities)(_.identifier)
    def * = (entityIdentifier, extendsIdentifier)
  }
  val entityExtends = TableQuery[MDREntityExtend]

  def recreate = {
    DB.withSession { implicit session =>
      val stmt = session.conn.createStatement();
      stmt.execute("SET FOREIGN_KEY_CHECKS=0")
      stmt.execute("drop table if exists mdrentity ")
      stmt.execute("drop table if exists mdrattribute")
      stmt.execute("drop table if exists mdrpackage")
      stmt.execute("drop table if exists mdrentityextends")
      stmt.execute("SET FOREIGN_KEY_CHECKS=1")
      stmt.close()
      (entities.ddl ++ attributes.ddl ++ packages.ddl ++ entityExtends.ddl).create
      //packages += ("","","",Some(""),"")
    }
    
  }
 }

object ModelDB {
  def main(args: Array[String]): Unit = {
  
    val DB = new ModelDB
    DB.recreate

  }

}
