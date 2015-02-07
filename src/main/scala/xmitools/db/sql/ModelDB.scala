package xmitools.db.sql
import scala.slick.driver.MySQLDriver.simple._

class ModelDB {

  val db_user = "molgenis"
  val db_password = "molgenis"
  val db_uri = "jdbc:mysql://localhost/omx"

  
  val DB = Database.forURL(url = db_uri, user = db_user, password = db_password, driver = "com.mysql.jdbc.Driver")

  class MDRPackage(tag: Tag) extends Table[(String, String, String, Option[String], Option[String], String)](tag, "MDRPackage") {
    def identifier = column[String]("identifier", O.PrimaryKey)
    def sourceIdentifier = column[String]("sourceIdentifier")
    def namespaceIdentifier = column[String]("namespaceIdentifier")
    def name = column[String]("name")
    def description = column[Option[String]]("description",O.DBType("TEXT"))
    def parentPackageIdentifier = column[Option[String]]("parentPackageIdentifier")
    def pkgIdx = index("packagePackageIdx", (parentPackageIdentifier), unique = false)
    def * = (identifier, name, sourceIdentifier, description, parentPackageIdentifier, namespaceIdentifier)
  }
  val packages = TableQuery[MDRPackage]

  class MDREntity(tag: Tag) extends Table[(String, String, String, Option[String], String, String, String, Option[String])](tag, "MDREntity") {
    def identifier = column[String]("identifier", O.PrimaryKey)
    def sourceIdentifier = column[String]("sourceIdentifier")
    def name = column[String]("name")
    def description = column[Option[String]]("description",O.DBType("TEXT"))
    def typeCv = column[String]("typeCv")
    def typeQualifierCv = column[String]("typeQualifierCv")
    def packageIdentifier = column[String]("packageIdentifier")
    def instanceOfIdentifier = column[Option[String]]("instanceOfIdentifier") //typeCv == instance
    //def pkgFK = foreignKey("packageFF", packageIdentifier, packages)(_.identifier)
    def pkgIdx = index("entityPackageIdx", (packageIdentifier), unique = false)
    def * = (identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, packageIdentifier, instanceOfIdentifier)
  }
  val entities = TableQuery[MDREntity]

  class MDRAttribute(tag: Tag) extends Table[(String, String, String, Option[String], String, String, String, String, Option[String], Option[String], Option[String], Boolean, Option[String])](tag, "MDRAttribute") {
    def identifier = column[String]("identifier", O.PrimaryKey)
    def sourceIdentifier = column[String]("sourceIdentifier")
    def name = column[String]("name")
    def typeCv = column[String]("typeCv")
    def typeQualifierCv = column[String]("typeQualifierCv")
    def description = column[Option[String]]("description",O.DBType("TEXT"))
    def entityIdentifier = column[String]("entityIdentifier")
    def classifierIdentifier = column[String]("classifierIdentifier") // must be same as associationIdentifier in N-aray cases
    def associationIdentifier = column[Option[String]]("associationIdentifier")
    //def entityFK = foreignKey("attribEntityFK", entityIdentifier, entities)(_.identifier)
    //def typeFK = foreignKey("attribTypeFK", typeIdentifier, entities)(_.identifier)
    def attributeIdx = index("attribEntityIdx", (entityIdentifier), unique = false)
    def classifIdx = index("attribClassifIdx", (classifierIdentifier), unique = false)
    def lowerBound = column[Option[String]]("lowerBound")
    def upperBound = column[Option[String]]("upperBound")
    def navigable = column[Boolean]("navigable")
    def aggregation = column[Option[String]]("aggregation")
    def * = (identifier, name, sourceIdentifier, description, typeCv, typeQualifierCv, entityIdentifier, classifierIdentifier, associationIdentifier, lowerBound, upperBound, navigable, aggregation)
  }

  val attributes = TableQuery[MDRAttribute]

  class MDREntityExtend(tag: Tag) extends Table[(String, String)](tag, "MDREntityExtends") {
    def entityIdentifier = column[String]("entityIdentifier")
    def extendsIdentifier = column[String]("extendsIdentifier")
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
      // Dummy entities are used in cases where classifiers of attributes/associations are not resolved
      // The entity do not belong to any package and is not selected in typical queries
      import ModelDB.DUMMY_ID
      entities += (DUMMY_ID, DUMMY_ID, DUMMY_ID, None, "dummy", "dummy", DUMMY_ID,None)
      
    }
    
  }
 }

object ModelDB {
   final val DUMMY_ID = "---DUMMY---"
 
  def main(args: Array[String]): Unit = {
  
    val DB = new ModelDB
    DB.recreate

  }

}
