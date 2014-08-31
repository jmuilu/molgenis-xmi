package xmitools.model
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import com.typesafe.scalalogging.slf4j.LazyLogging

class EclipseXMIVersionHandler(XMI: Ns, UML: Ns, version: String = "") extends XMIVersionHandler(XMI, UML, version) {

  import Ns._
  override def getRootPackageNode(elem: Elem): Node = {
    return elem;
  }

  override def parseId(currentNode: XNode): String = {
    val n = currentNode.n;
    val id = n.attribute(XMI, "id").get
    assert(!id.isEmpty, "XMI element do not have id")
    return id.text
  }

  override def parseAttributeType(currentNode: XNode): Option[String] = {
    val n = currentNode.n
    val typeref = n.attribute("type");
    if (!typeref.isDefined) {
      return None
    } else {
      return Some(typeref.get.text)
    }
  }

  override def parseNodeType(currentNode: XNode): Option[String] = {
    if (currentNode.n.label == "Model" && currentNode.n.attribute(XMI, "type") == None) {
      return Some("uml:Model")
    } else {
      val r = toOptStr(currentNode.n.attribute(XMI, "type"))
      if (!r.isDefined) {
        //logger.warn("Node type is undefined in node: "+currentNode.id)
      }
      return r
    }
  }

  override def parseRealizations(currentNode: XNode): List[XRealization] = {
    val res = (currentNode.n \ "interfaceRealization").map {
      a => XRealization(a, Some(currentNode))(this)
    }
    return res.toList
  }

  override def parseMemberEndIdRefs(currentNode: XNode): Seq[String] = {
    val n = currentNode.n
    val ids = n.attribute("memberEnd");
    if (!ids.isDefined) {
      return Seq()
    } else {
      return ids.get.text.split("\\s+")
    }
  }

  override def parseIdRefOfOwnedComment(currentNode: XNode): Option[String] = {
    val x = currentNode.n.attribute("annotatedElement")
    return toOptStr(x)
  }

  override def parseOwnedCommentBody(currentNode: XNode): Option[String] = {
    val res = (currentNode.n \ "body")
    if (res.isEmpty) {
      return None
    } else {
      return Some(res.text)
    }
  }

  override def parseNavigable(currentNode: XNode): Option[Boolean] = {
    val parent = getParent(currentNode)
    val parentType = parent.nodeType
    if (currentNode.n.label != "ownedEnd") {
      //todo: EA does always like this ... re-check!
      if (currentNode.n.label == "ownedAttribute") {
        if (parentType.isDefined && eq2(parentType, UML, "Class")) {
          return Some(true)
        } else {
          return Some(false)
        }
      } else {
        return None
      }
    } else {
      // parent here is  uml:Association
      assert(parent.nodeType.isDefined && eq2(parentType, UML, "Association"), "Parent node should be an association. Found " + parent.nodeType + " id = " + currentNode.id)
      val ends = parent.n.attribute("navigableOwnedEnd")
      if (ends.isDefined) {
        if (ends.isEmpty) {
          return None
        } else {
          if (ends.get.text.split("\\s+").filter(f => f == currentNode.id).length > 0) {
            return Some(true)
          } else {
            return Some(false)
          }
        }
      } else {
        return None
      }
    }
  }

  override def parsePackagesFromExtensions(rootNode: Node, currentNode: XNode): List[XPackage] = List()

}

