package xmitools.model

import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import com.typesafe.scalalogging.slf4j.LazyLogging

/* Simple XMI wrapper. 
 * ===================
 * 
 * See:
 * http://www.objectsbydesign.com/projects/xmi_to_html_2.html
 * http://publib.boulder.ibm.com/infocenter/rsmhelp/v7r0m0/index.jsp?topic=/com.ibm.xtools.modeler.doc/topics/cdepend.html
 * 
 * http://www.uml-diagrams.org/
 * http://www.uml-diagrams.org/association.html
 * 
 * http://www.omg.org/spec/UML/2.4.1/
 * http://www.omg.org/spec/UML/2.4.1/Superstructure/PDF
 * http://www.omg.org/spec/XMI/
 * 
 * Notes:
 * Parsing will be moved into separate version handler, when need arises.
 * 
 * */

trait DirectedRelationship {
  val targetId: String
  val sourceId: String

  def parentId = targetId
  def childId = sourceId
}


/*
 * Namespace utility class
 */
case class Ns(abbreviation: String, uri: String) {
  override def toString(): String = uri
}

/*
 * Utility methods 
 */
object Ns {

  implicit def x2x(x: Ns): String = x.uri;

  def eq2(x: Option[String], ns: Ns, str: String): Boolean = {
    val res = x.isDefined && x.get == (ns.abbreviation + ":" + str)
    return res
  }

  def eqs(x: Option[Seq[Node]], ns: Ns, str: String) = x.isDefined && (x.get.text == (ns.abbreviation + ":" + str))

  def toOptStr(x: Option[Seq[Node]]): Option[String] = { if (x.isDefined && !x.get.isEmpty) { return Some(x.get.text) } else None }
  
  def getTextOrDie(n: Node, name: String): String = {
    val x = (n \ name)
    assert(!x.isEmpty, "Assertion failed. Property " + name + " do not exist in node ")
    return x.text
  }
  
  def exist(x: Option[Seq[Node]]) = x.isDefined && !x.get.text.isEmpty()

  def s(ns: Ns, str: String) = "@{" + ns.toString + "}" + str
}

/*
 * UML / XMI Classes
 *
 */

trait XNode extends LazyLogging {

  val n: Node
  val id: String
  val name: Option[String]
  val nodeType: Option[String]
  val parent: Option[XNode]
  def children: List[XNode]
  def allChildren: List[XNode]
  def qualifiedName: Option[String]
  val comments: List[XComment] //comment elements which are in packakeable element. Comments have reference to a entity they are annotating

}

case class XNodeImpl(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNode {

  import Ns._

  lazy val id = vs.parseId( this)

  lazy val name = toOptStr(n.attribute("name"))
  
  lazy val nodeType = vs.parseNodeType(this);

  lazy val comments = (n \ "ownedComment").map {
    a => XComment(a, parent)
  }.toList

  override def toString(): String = name + " " + nodeType + " " + id

  def children: List[XNode] = comments

  def packagedElementsFromExtensions: List[XPackage] = List()

  def allChildren: List[XNode] = {
    val rest = children.flatMap { c => c.allChildren }
    return children ++ rest
  }
  
  def qualifiedName = {
    if ( ! name.isDefined) {
      None
    } else {
      if ( parent.isDefined) {
        if ( parent.get.qualifiedName.isDefined) {
          Some( parent.get.qualifiedName.get +"." + name.get)
        } else {
          None
        }
      } else {
        Some( name.get )
      }
    }
  }
}

class XPackage(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) {
  import Ns._

  assert(nodeType.isDefined && nodeType.get == "uml:Package" || nodeType.get == "uml:Model", "Node should be a package or model. Found " + nodeType + " id = " + this.id)
  lazy val packagedElements = (n \ "packagedElement").map {
    e =>
      if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Class")) {
        Some(XClass(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Package")) {
        Some(XPackage(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Interface")) {
        Some(XInterface(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Association")) {
        Some(XAssociation(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "AssociationClass")) {
        Some(XAssociationClass(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Realization")) {
        None //Some(XRealization(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Dependency")) { //todo: check should this be moved away like XRealizations
        Some(XDependency(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Artifact")) {
        Some(XArtifact(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "Enumeration")) {
        Some(XEnumeration(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "PrimitiveType")) {
        Some(XPrimitiveType(e, Option(this)))
      } else if (eqs(e.attribute(vs.XMI, "type"), vs.UML, "DataType")) {
        Some(XDataType(e, Option(this)))
      } else {
        None
      }
  }.flatten ++ packagedElementsFromExtensions

  lazy val subPackages = packagedElements.filter(f => f.isInstanceOf[XPackage]).map { f => f.asInstanceOf[XPackage] }.toList
  lazy val entities = packagedElements.filter(f => f.isInstanceOf[XClassifier]).map { f => f.asInstanceOf[XClassifier] }.toList
  lazy val dataTypes = packagedElements.filter(f => f.isInstanceOf[XDataType]).map { f => f.asInstanceOf[XDataType] }.toList
  lazy val allSubPackages = subPackages.flatMap(p => toPackageList(p)).toList
  lazy val allPackages =  this :: allSubPackages 
  lazy val associations = packagedElements.filter(f => f.isInstanceOf[XAssociation]).map { f => f.asInstanceOf[XAssociation] }.toList
  //lazy val realizations = packagedElements.filter(f => f.isInstanceOf[XRealization]).map { f => f.asInstanceOf[XRealization] }.toList
  lazy val dependencies = packagedElements.filter(f => f.isInstanceOf[XDependency]).map { f => f.asInstanceOf[XDependency] }.toList

  override def children: List[XNode] = super.children ++ packagedElements

  private def toPackageList(p: XPackage): List[XPackage] = {
    val subPackages = p.subPackages.flatMap(s => toPackageList(s))
    return List(p) ++ subPackages.toList
  }

}

object XPackage {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XPackage(n, parent)(vs)
}

abstract class XClassifier(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) {

  lazy val attributes = (n \ "ownedAttribute").map {
    a => XProperty(a, Some(this))
  }.toList

  lazy  val generalizations = (n \ "generalization").map {
    a => XGeneralization(a, Some(this))
  }.toList

  lazy val realizations = vs.parseRealizations(this)

  override def children: List[XNode] = super.children ++ attributes ++ generalizations ++ realizations

  val entityType: String // convenience attribute (compatible with model-reg)

  lazy val isAbstract = vs.parseIsAbstract(this)
  
  def parentIds = {
    ( generalizations.asInstanceOf[List[DirectedRelationship]] ++ realizations.asInstanceOf[List[DirectedRelationship]] ) . map {
      p=>
      assert( this.id == p.childId,"XMI Parent / Child relationsship do  not match. See "+ this.id+" "+p.childId ) 
      p.parentId
    }
  }
}

class XClass(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  val entityType = "class"
}

object XClass {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XClass(n, parent)(vs)
}

class XInterface(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  override val entityType = "interface"
}

object XInterface {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XInterface(n, parent)(vs)
}

class XArtifact(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  override val entityType = "artifact"
}
object XArtifact {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XArtifact(n, parent)(vs)
}

/*
 * Primitive types are taken e.g. from XMI extensions package. Has type uml:PrimitiveType
 */
class XPrimitiveType(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  override val entityType = "primitive"
}

object XPrimitiveType {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XPrimitiveType(n, parent)(vs)
}
/*
 * DataTypes corresponds to uml:DataType. 
 */
class XDataType(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  override val entityType = "primitive"
}
object XDataType {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XDataType(n, parent)(vs)
}

class XEnumeration(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  override val entityType = "enumeration"
}
object XEnumeration {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XEnumeration(n, parent)(vs)
}

class XAssociation(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XClassifier(n, parent)(vs) {
  import Ns._

  val memberEndIdRefs = vs.parseMemberEndIdRefs(this);

  val ownedEnds = (n \ "ownedEnd").map { node => XProperty(node, Some(this)) }

  override def children = super.children ++ ownedEnds

  override val entityType = "association"
}

object XAssociation {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XAssociation(n, parent)(vs)
}

class XAssociationClass(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XAssociation(n, parent)(vs) {
  override val entityType = "associationClass"
}
object XAssociationClass {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XAssociationClass(n, parent)(vs)
}

class XGeneralization(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) with DirectedRelationship {
  import Ns._

  /* Cases like: 
   * <generalization xmi:type="uml:Generalization" xmi:id="view_table_generalization" general="table"/>
   * Or
   * <generalization xmi:type="uml:Generalization" xmi:id="EAJava_boolean_General">
   *   <general href="http://schema.omg.org/spec/UML/2.1/uml.xml#Boolean"/>
   * </generalization>
   * 
   */
  val targetId = if (exist(n.attribute("general"))) n.attribute("general").get.text else {
    val x = (n \ "general" \ "@href")
    assert(!x.isEmpty, "Cannot parse genralization in node " + n.attribute(vs.XMI, "id").get)
    x.text
  }

  val sourceId = parent.get.id
}
object XGeneralization {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XGeneralization(n, parent)(vs)
}

class XRealization(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) with DirectedRelationship {
  import Ns._
  val targetId = getTextOrDie(n, "@supplier") // parent
  val sourceId = getTextOrDie(n, "@client") // child
}
object XRealization {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XRealization(n, parent)(vs)
}

class XDependency(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) with DirectedRelationship {
  import Ns._
  //client depends on supplier client --> supllier 
  val targetId = getTextOrDie(n, "@supplier")
  val sourceId = getTextOrDie(n, "@client")
}

object XDependency {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XDependency(n, parent)(vs)
}

class XComment(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) {
  import Ns._
  val refersTo = vs.parseIdRefOfOwnedComment(this)
  val body = vs.parseOwnedCommentBody(this);
}

object XComment {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XComment(n, parent)(vs)
}

/* A StructuralFeature */
class XProperty(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) extends XNodeImpl(n, parent)(vs) {

  import Ns._

  def isNavigable: Option[Boolean] = {
    //todo: study do we need to call this (must be in ownedEnd)
    vs.parseNavigable(this);
  }

  val typeIdRef: Option[String] = {
    vs.parseAttributeType(this);
  }

  val lowerValue: Option[String] = {
    val v = (n \ "lowerValue" \ "@value");
    if (v.isEmpty) None;
    else Some(v.text)
  }

  val upperValue: Option[String] = {
    val v = (n \ "upperValue" \ "@value");
    if (v.isEmpty) None;
    else Some(v.text)
  }

  val aggregation: Option[String] = {
    val v = (n \ "@aggregation");
    if (v.isEmpty) None;
    else Some(v.text)
  }

  val association: Option[String] = {
    //attribute can be also association. This happens when association end is given as navigable
    val a = (n \ "@association");
    if (a.isEmpty) None;
    else Some(a.text)
  }

  override def toString(): String = super.toString + " " + typeIdRef

}

object XProperty {
  def apply(n: Node, parent: Option[XNode])(implicit vs: XMIVersionHandler) = new XProperty(n, parent)(vs)
}

/*
 * XMILoader
 * 
 */
class UMLModel private (modelNode: Node, rootNode: Node)(implicit vs: XMIVersionHandler) extends XPackage(modelNode, None)(vs) {

  import Ns._

  override def packagedElementsFromExtensions = {
    vs.parsePackagesFromExtensions(rootNode, this)
  }

  val createResolver = new IDResolver(this, vs)

}

object UMLModel {

  def apply(x: Elem)(implicit vs: XMIVersionHandler) = {
    assert(vs != null)
    assert(x != null)
    assert(x.scope != null)
    assert(x.scope.getURI("uml") == vs.UML.uri, " XMI Not supported. URI = " + x.scope.getURI("uml") + ". Should have been=" + vs.UML.uri)
    assert(x.scope.getURI("xmi") == vs.XMI.uri, " XMI Not supported. XMI = " + x.scope.getURI("xmi"))
    if ( vs.version != "") {
      val v = x.attribute(vs.XMI, "version")
      assert( v.isDefined,"XMI not supported. Wrong version" )
      assert( v.get.text == vs.version,"XMI not supported. Wrong version "+v.get.text )
    }
    new UMLModel(vs.getRootPackageNode(x), x)(vs)
  }


}