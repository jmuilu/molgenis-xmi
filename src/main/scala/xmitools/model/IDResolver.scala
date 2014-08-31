package xmitools.model

import com.typesafe.scalalogging.slf4j.LazyLogging

/*
 * IDResolver
 * 
 */


class IDResolver(n: XPackage, vs: XMIVersionHandler) extends LazyLogging {

  private def mapChildren(children: List[XNode]): List[(String, XNode)] = {

    val childrenList: List[(String, XNode)] = children.map {
      f =>
        if (f.children.size > 0) {
          List((f.id, f.asInstanceOf[XNode])) ++ mapChildren(f.children)
        } else {
          List((f.id, f.asInstanceOf[XNode]))
        }
    }.flatten 

    return childrenList
  }

  lazy val idMap = createIdMap(n)

  def associationMap(n: XPackage) = {
	  
    //take this package as well
    (n :: n.allSubPackages).map {
      p =>
        p.associations.map {
          a =>
            a.memberEndIdRefs
        }

    }

  }

  lazy val otherDocumentationMap = digOutOtherDocumentations(n)

  //todo: move these to version handler
  private def digOutOtherDocumentations(n: XPackage): Map[String, Seq[String]] = {
    import Ns._
    val list = (n.n \\ "element").filter(p => p.attribute(vs.XMI, "idref").isDefined).flatMap {
      e =>
        val id = e.attribute(vs.XMI, "idref")
        val list =
          (e \ "properties" \ "@documentation").map { doc => doc.text }.filter(p => (p + "").trim != "")

        val list2 = (e \ "attributes" \ "attribute").filter(p => p.attribute(vs.XMI, "idref").isDefined).map {
          a =>
            val id = a.attribute(vs.XMI, "idref")
            val list =
              (a \ "documentation" \ "@value").map { doc => doc.text }.filter(p => (p + "").trim != "")
            (id.get.text, list)
        }.filter(p => p._2.size > 0)

        if (list.size > 0) {
          Seq((id.get.text, list)) ++ list2
        } else {
          list2
        }
    }
    return Map(list: _*)
  }

  private def createIdMap(n: XPackage): Map[String, XNode] = {
    val xList = (n.id, n.asInstanceOf[XNode]) :: n.allChildren.map{c=>(c.id,c) }
    //check:
    for (i <- 0 until xList.size - 1) {
      for (j <- i + 1 until xList.size) {
        if (xList(i)._1.equals(xList(j)._1)) {
          logger.warn("Check entity:  " + xList(i)._1 + ". It has introduced in XMI more than once. toString: " + xList(i)._2 + " AND " + xList(j)._2)
        }
      }
    }
    return Map(xList: _*)
  }

  lazy val commentMap = createCommentMap(n)
  /*
   * Find all comments XNode may have
   */
  private def createCommentMap(n: XNode): Map[String, List[XComment]] = {

    /* dig out all comments elements may have and extract xnodes (node ids) they are refering to: */
    val list1 = n.comments.flatMap(f => f.refersTo.map(x => (x, f)))
    val list2 = n.allChildren.flatMap(c => c.comments.flatMap(f => f.refersTo.map(x => (x, f))))
    val all = list1 ++ list2
    /* take distinct xnode ids and filter out comments for them: */
    val distinctNodeIds = all.map(f => f._1).distinct //todo: use groupBy
    val l = distinctNodeIds.map {
      id =>
        (id, all.filter(f => f._1 == id).map { s => s._2 })
    }
    return Map(l: _*)
  }

   def createSubClassMap(x: XPackage): Map[String, List[DirectedRelationship]] = {

    // remeber to take this package as well     
    val generalizations = (x :: x.allSubPackages).flatMap(f => f.entities.flatMap { e => e.generalizations.asInstanceOf[List[DirectedRelationship]] }) 
    val realizations = (x :: x.allSubPackages).flatMap(f => f.entities.flatMap { e => e.realizations .asInstanceOf[List[DirectedRelationship]] }) 
    val list = (realizations ++ generalizations).map {
      r =>
        (r.parentId, r)
    }
    val distinctIds = list.map(f => f._1).distinct
    val l = distinctIds.map {
      id => (id, list.filter(f => f._1 == id).map(p => p._2))
    }

    Map(l: _*)
  }
  lazy val subClassMap = createSubClassMap(n)


  def resolve(id: String): Option[XNode] = {
    return idMap.get(id)
  }

  def resolveComments(id: String): List[XComment] = {

    val comm = if (!commentMap.isDefinedAt(id)) {
      return List()
    } else {
      commentMap.get(id).get
    }
    return comm
  }

  /* Resolves other end of an association. Association ends are attributes (uml:Property) which are either 
   * by an Entity (if they are navigable) or Association (non navigable associations)*/
  def resolveOtherEndOfBinaryAssociation(associationId: String, memberId: String): Option[XProperty] = {
    val _x = resolve(associationId)
    if (_x.isDefined) {
      val x = _x.get
      assert(x.isInstanceOf[XAssociation], "Entity " + associationId + " is not association. Entity: " + x)
      val asso = x.asInstanceOf[XAssociation]
      if (asso.memberEndIdRefs.size == 2) {
        val otherEnds = asso.memberEndIdRefs.filter(p => p != memberId)
        if (otherEnds.size == 1) {
          val xOther = resolve(otherEnds.head)
          if (xOther.isDefined) {
            if (xOther.get.isInstanceOf[XProperty]) {
              return Some(xOther.get.asInstanceOf[XProperty])
            } else {
              assert(false, "Something wrong in association " + associationId + " Member end not attribute: " + otherEnds.head)
              return None
            }
          } else {
            assert(false, "Something wrong in association " + associationId + " Member end not resolved: " + otherEnds.head)
            return None
          }
        } else {
          assert(false, "Cannot resolve association end. Association " + associationId + " is not valid")
          return None
        }
      } else {
        logger.warn("Cannot resolve association end. Association " + associationId + " is not binary")
        return None
      }
    } else {
      assert(false, "Cannot resolve association end. Association " + associationId)
      return None

    }
  }

  def resolveSubClasses(id: String): List[DirectedRelationship] = {

    if (!subClassMap.isDefinedAt(id)) {
      return List()
    } else {
      subClassMap.get(id).get
    }
  }

  def subClassesAsCsvString(prefix: String, id: String): String = {
    resolveSubClasses(id).map(s => prefix + s.childId).mkString(",")
  }

  def commentsAsText(id: String): String = {
    return resolveComments(id).map(p => p.body).flatten.mkString(" | ").replaceAll("\t", " ").replaceAll("\r?\n", " ") // todo: tune this
  }
  def allCommentsAsText(id: String): String = {
    val otherDocs = otherDocumentationMap.getOrElse(id, List()).mkString(" | ")
    return (otherDocs + resolveComments(id).map(p => p.body).flatten.mkString(" | ")).replaceAll("\t", " ").replaceAll("\r?\n", " ") // todo: tune this
  }

}
