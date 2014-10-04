package xmitools.model
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import com.typesafe.scalalogging.slf4j.LazyLogging

/*
 * Version handler for EA UML/XMI 2.4.1
 */
class XMIVersionHandler(val XMI: Ns, val UML: Ns, val version : String = "") extends LazyLogging {

  val DUMMYID = "DUMMY_ID"
  var rootId:String = null;
  import Ns._

  def setRootId( id: String ) = {
    rootId = id
  }
  
  def getRootPackageNode(elem: Elem): Node = {
    return (elem \ "Model").head;
  }

  def parseId(currentNode: XNode) = {
    val n = currentNode.n;
    if (!n.attribute(XMI, "id").isDefined) {
      assert(n.label == "Model", "ID is undefined") 
      if ( rootId!= null ) {
         rootId 
      } else {
        logger.warn("Model do not have id. Dummy id assigned: " + DUMMYID)
        DUMMYID
      }
    } else {
      n.attribute(XMI, "id").get.text
    }
  }

  def getParent(currentNode: XNode): XNode = {
    assert(currentNode.parent.isDefined, "Parent element must be defined. Error in element: " + currentNode.id)
    return currentNode.parent.get
  }

  def parseNodeType(currentNode: XNode): Option[String] = toOptStr(currentNode.n.attribute(XMI, "type"))

  def parseAttributeType(currentNode: XNode): Option[String] = {
    val n = currentNode.n
    val typeref = (n \ "type" \ s(XMI, "idref"));
    if (typeref.isEmpty) None;
    else Some(typeref.text)

  }

  def parseRealizations(currentNode: XNode): List[XRealization] = {
    //realizations are in parent package
    assert(currentNode.parent.isDefined, "Parent pkg not defined for element: " + currentNode.id)
    //take only those realization's which belong to the currentNode
    (currentNode.parent.get.n \ "packagedElement").
      filter(e => eqs(e.attribute(XMI, "type"), UML, "Realization")).
      filter(e => getTextOrDie(e, "@client") == currentNode.id).map {
        r =>
          XRealization(r, Some(currentNode))(this)
      }.toList
  }

  def parseIdRefOfOwnedComment(currentNode: XNode): Option[String] = {
    val x = (currentNode.n \ "annotatedElement" \ s(XMI, "idref")).map(e => e.text)
    return x.headOption
  }

  def parseOwnedCommentBody(currentNode: XNode): Option[String] = {
    val res = (currentNode.n \ "@body")
    if (res.isEmpty) {
      return None
    } else {
      return Some(res.head.text)
    }
  }

  def parseMemberEndIdRefs(currentNode: XNode): Seq[String] = {

    val n = currentNode.n
    val memberEndIdRefs = (n \ "memberEnd").map {
      nodes =>
        val ref = nodes.attribute(XMI, "idref")
        assert(ref.isDefined, "Attribute idref do not exist in memberEnd " + nodes.attribute(XMI, "id"))
        ref.get.text
    }
    memberEndIdRefs;
  }

  def parseIsAbstract(currentNode: XNode): Option[Boolean] = {
    val abs = currentNode.n.attribute("isAbstract")
    val res = if (abs.isDefined) {
      if (abs.get.text == "true") {
        Some(true)
      } else if (abs.get.text == "false") {
        Some(false)
      } else {
        logger.warn("UKNOWN Boolean value ( isAbstract attribute)): " + abs.get.text + ". In node " + currentNode.id)
        None
      }
    } else {
      None
    }
    return res
  }

  def parseNavigable(currentNode: XNode): Option[Boolean] = {
    val parent = getParent(currentNode)
    val parentType = parent.nodeType
    //todo: check with Eclipse implementation. Should we check Association classes as well    
    val navigability =
      if (parentType.isDefined && eq2(parentType, UML, "Class")) {
        //In EA XMI 2.4.1 this seems to be the case for ownEnd associations, which are part of the Class's properties with attributes.
        //Attributes should always be navigable by definition
        Some(true)
      } else {
        Some(false)
      }
    return navigability
  }

  def parsePackagesFromExtensions(rootNode: Node, currentNode: XNode): List[XPackage] = {
    //extensions are in parent node
    (rootNode \ "Extension" \ "primitivetypes" \ "packagedElement").
      filter(node => eqs(node.attribute(XMI, "type"), UML, "Package")).map {
        p => XPackage(p, Some(currentNode))(this)
      }.toList
  }
}

object XMIVersionHandler {

  val EA_XMI241 = new XMIVersionHandler(Ns("xmi", "http://www.omg.org/spec/XMI/20110701"), Ns("uml", "http://www.omg.org/spec/UML/20110701"))
  val ECLIPSE_20110701 = new EclipseXMIVersionHandler(Ns("xmi", "http://www.omg.org/spec/XMI/20110701"), Ns("uml", "http://www.eclipse.org/uml2/4.0.0/UML"))
  val ECLIPSE_2_0 = new EclipseXMIVersionHandler(Ns("xmi", "http://www.omg.org/XMI"), Ns("uml", "http://www.eclipse.org/uml2/4.0.0/UML"),"2.0")
  def apply() = new XMIVersionHandler(Ns("xmi", "http://www.omg.org/spec/XMI/20110701"), Ns("uml", "http://www.omg.org/spec/UML/20110701"))
}