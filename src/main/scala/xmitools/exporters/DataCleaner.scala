package xmitools.exporters

import com.typesafe.scalalogging.slf4j.LazyLogging
import xmitools.model.XClassifier
import xmitools.model.DirectedRelationship
import xmitools.model.XProperty
import java.beans.Introspector
import xmitools.model.XNode

trait DataCleaner extends LazyLogging {

  val UNKNOWN = "undefined"
  val UNBOUND = "-1"
  def P(x: String): String = x.replaceAll("\\s+", " ")
  def M(x: String): String = if (x == "*") UNBOUND else x //unlimited multiplicity

  def isMoreThanOne(multiplicity: Option[String]): Boolean = {
    if (multiplicity.isDefined) {
      val m = multiplicity.get
      if (m == "*") {
        true
      } else {
        if (m.matches("\\d+") && Integer.parseInt(m) > 1) {
          true
        } else {
          false
        }
      }
    } else {
      false
    }
  }

  def isZero(multiplicity: Option[String]): Boolean = {
    if (multiplicity.isDefined) {
      val m = multiplicity.get
      if (m == "*") {
        true
      } else {
        if (m.matches("\\d+") && Integer.parseInt(m) == 1) {
          true
        } else {
          false
        }
      }
    } else {
      false
    }
  }

  def getOpt(o: Option[String]): String = {
    if (o.isDefined) {
      return o.get
    } else {
      return UNKNOWN
    }
  }

  def toOpt(str: String): Option[String] = {
    if (str == null || str == "") {
      None
    } else {
      Some(str)
    }
  }

  def deriveRoleNameForAssociationEnd(p: XProperty , typeEntity: XNode): String = {
    if (!p.name.isDefined) {
      if (p.isNavigable.isDefined && p.isNavigable.get) {
        val _name = Introspector.decapitalize(typeEntity.name.get)
        logger.warn("Role name of navigable association end " + p.id + " is undefined. Attribute (xref) name will be set to " + _name)
        _name
      } else {
        UNKNOWN
      }
    } else {
      p.name.get
    }
  }
  
}