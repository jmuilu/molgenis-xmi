package xmitools

import org.scalatest.FunSuite
import xmitools.model.UMLModel
import org.scalatest.Assertions._
import xmitools.model.XMIVersionHandler
import xmitools.model.Ns
import xmitools.model.XClass
import xmitools.model.XInterface
import xmitools.model.XAssociationClass
import xmitools.model.XProperty

class ModelTest extends FunSuite {

  abstract class Fixture {

    val file: String
    val vsHandler: XMIVersionHandler
    val xmi = scala.xml.XML.loadFile(file)
    def model = UMLModel(xmi)(vsHandler)
    val r = model.createResolver

  }

  class TestFixture(val file: String, val vsHandler: XMIVersionHandler) extends Fixture {
  }

  val eaFixture = new TestFixture("src/test/resources/models/TEST_MODEL_UML241.xmi", XMIVersionHandler.EA_XMI241)

  val textUMlFixture = new TestFixture("src/test/resources/models/model_reg.uml", XMIVersionHandler.ECLIPSE_20110701)

  val genMyModelFixture = new TestFixture("src/test/resources/models/MYGENMODEL_TEST.xmi", XMIVersionHandler.ECLIPSE_2_0)

  val fixtures = List(eaFixture, textUMlFixture, genMyModelFixture)

  test("test navigability 1 TextUML/ECLIPSE XMI 201110701") {

    val f = textUMlFixture

    val a1 = f.r.resolve("stewardship-stewardedItem")
    assert(a1.isDefined)
    assert(a1.get.isInstanceOf[XProperty])
    val a2 = f.r.resolveOtherEndOfBinaryAssociation("stewardship", a1.get.id)
    assert(a2.isDefined)
    assert(a2.get.isInstanceOf[XProperty])

    val asso1End = a1.get.asInstanceOf[XProperty];
    val asso2End = a2.get.asInstanceOf[XProperty];
    assert(asso1End.name.isDefined)
    assert(asso2End.name.isDefined)
    assert(asso1End.name.get == "stewardedItem")
    assert(asso2End.name.get == "stewardshipRecord")
    assert(!asso1End.lowerValue.isDefined) //there is error in textUML. This should be 0
    assert(asso1End.upperValue.isDefined)
    assert(asso1End.upperValue.get == "*")
    assert(asso1End.isNavigable.isDefined)
    assert(asso1End.isNavigable.get)
    assert(asso2End.lowerValue.isDefined)
    assert(asso2End.lowerValue.get == "1")
    assert(asso2End.upperValue.isDefined)
    assert(asso2End.upperValue.get == "1")
    assert(asso2End.isNavigable.isDefined)
    assert(asso2End.isNavigable.get)

  }

  test("test navigability 2 TextUML/ECLIPSE XMI 201110701") {

    val f = textUMlFixture

    val a1 = f.r.resolve("registration-authority")
    assert(a1.isDefined)
    assert(a1.get.isInstanceOf[XProperty])
    val a2 = f.r.resolveOtherEndOfBinaryAssociation("registration", a1.get.id)
    assert(a2.isDefined)
    assert(a2.get.isInstanceOf[XProperty])

    val asso1End = a1.get.asInstanceOf[XProperty];
    val asso2End = a2.get.asInstanceOf[XProperty];
    assert(asso1End.name.isDefined)
    assert(asso2End.name.isDefined)
    assert(asso1End.name.get == "authority")
    assert(asso2End.name.get == "registrar")
    assert(!asso1End.lowerValue.isDefined) //there is error in textUML. This should be 0
    assert(asso1End.upperValue.isDefined)
    assert(asso1End.upperValue.get == "1")
    assert(asso1End.isNavigable.isDefined)
    assert(asso1End.isNavigable.get)
    assert(!asso2End.lowerValue.isDefined)
    assert(asso2End.upperValue.isDefined)
    assert(asso2End.upperValue.get == "1")
    assert(asso2End.isNavigable.isDefined)
    assert(asso2End.isNavigable.get)

  }

    test("test navigability 3 TextUML/ECLIPSE XMI 201110701") {

    val f = textUMlFixture

    val a1 = f.r.resolve("AdiministeredItem-authority")
    assert(a1.isDefined)
    assert(a1.get.isInstanceOf[XProperty])
    val a2 = f.r.resolveOtherEndOfBinaryAssociation("_packagedElement.9", a1.get.id)
    assert(a2.isDefined)
    assert(a2.get.isInstanceOf[XProperty])

    val asso1End = a1.get.asInstanceOf[XProperty];
    val asso2End = a2.get.asInstanceOf[XProperty];
    assert(asso1End.name.isDefined)
    assert(!asso2End.name.isDefined)
    assert(asso1End.name.get == "authority")
    assert(!asso1End.lowerValue.isDefined) //there is error in textUML. This should be 0
    assert(asso1End.upperValue.isDefined)
    assert(asso1End.upperValue.get == "*")
    assert(asso1End.isNavigable.isDefined)
    assert(asso1End.isNavigable.get)
    assert(!asso2End.lowerValue.isDefined)
    assert(!asso2End.upperValue.isDefined)
    assert(!asso2End.isNavigable.isDefined)

  }

  test("test GENMYMODEL  ECLIPSE XMI 2.0") {

    val f = genMyModelFixture
    assert(f.model.name.isDefined)
    assert(f.r.idMap.size == 34)
    assert(f.r.subClassMap.size == 2)

  }

  test("test EA UML 2.4.1 parser / abstract class and corresponding associations ") {

    val f = eaFixture

    val xnode = f.r.resolve("EAID_C071B323_3C5F_46a8_974B_A3303B66FA70")
    assert(xnode.isDefined, "Abstract class not found")
    assert(xnode.get.isInstanceOf[XClass], "Entity is not instance of XClass")

    val aClass = xnode.get.asInstanceOf[XClass]
    assert(aClass.name.isDefined, "Name of abstract class must be defined")
    assert(aClass.name.get == "Abstract class")
    assert(aClass.entityType == "class", "Entity type should have  been abstractClass. Was " + aClass.entityType)
    assert(aClass.isAbstract.get)
    /*
     *  we must have 2 ownedAttributes of which 2 are navigable associations (EA puts all navigable associations under XClass i.e. class owned associations) 
     */
    val attrib1 = aClass.attributes.filter { f => assert(f.name.isDefined); f.name.get == "test" }
    val attrib2 = aClass.attributes.filter { f => assert(f.name.isDefined); f.name.get == "class11 (target)" }
    val attrib3 = aClass.attributes.filter { f => assert(f.name.isDefined); f.name.get == "child" }

    assert(attrib1.length == 1)
    assert(attrib2.length == 1)
    assert(attrib3.length == 1)

    val a1 = attrib1.head;
    val a2 = attrib2.head;
    val a3 = attrib3.head;

    assert(a1.upperValue.isDefined)
    assert(a1.lowerValue.isDefined)

    assert(a1.typeIdRef.isDefined)
    assert(a2.typeIdRef.isDefined)
    assert(a3.typeIdRef.isDefined)

    assert(a1.association == None)
    assert(a2.association == Some("EAID_39356A47_3F4F_4321_AC2B_096DA6A8CD68"))
    assert(a3.association == Some("EAID_68719DDF_6B50_4631_8CF4_888DE35E0D5F"))

    val asso1 = f.r.resolve("EAID_39356A47_3F4F_4321_AC2B_096DA6A8CD68")
    val asso2 = f.r.resolve("EAID_68719DDF_6B50_4631_8CF4_888DE35E0D5F")
    assert(asso1.isDefined)
    assert(asso2.isDefined)

    assert(asso1.get.name.get == "Association Class")
    assert(!asso2.get.name.isDefined)

    val otherEndA = f.r.resolveOtherEndOfBinaryAssociation(asso1.get.id, a2.id)
    val otherEndB = f.r.resolveOtherEndOfBinaryAssociation(asso2.get.id, a3.id)
    assert(otherEndA.isDefined)
    assert(otherEndB.isDefined)

    //Type of other end should be this Class 
    assert(otherEndA.get.typeIdRef.get == "EAID_C071B323_3C5F_46a8_974B_A3303B66FA70")
    assert(otherEndB.get.typeIdRef.get == "EAID_C071B323_3C5F_46a8_974B_A3303B66FA70")

    //these ends are not EA-navigable and thus should be owned by the association class
    assert(otherEndA.get.parent.get.nodeType.get == "uml:AssociationClass")
    assert(otherEndB.get.parent.get.nodeType.get == "uml:Association")

    assert(otherEndA.get.isNavigable.isDefined)
    assert(otherEndB.get.isNavigable.isDefined)
    assert(!otherEndA.get.isNavigable.get)
    assert(!otherEndB.get.isNavigable.get)

    // other ends, i.e. here owned properties of XClass, must be navigable:
    assert(a2.isNavigable.isDefined)
    assert(a3.isNavigable.isDefined)
    assert(a2.isNavigable.get)
    assert(a3.isNavigable.get)

    val type1 = f.r.resolve(a1.typeIdRef.get)
    val type2 = f.r.resolve(a2.typeIdRef.get)
    val type3 = f.r.resolve(a3.typeIdRef.get)

    assert(type1.isDefined, "Cannot resolve type of association " + a1.id + " type = " + a1.typeIdRef.get)
    assert(type2.isDefined)
    assert(type3.isDefined)
    /*
     * Type's of (owned) associations are simply objects attribute is referencing to. Association property refers to XAssociation entity.
     * N-ary associations will be implemented differently. Likely type points to XAssociation entity and type == association
     */
    assert(type1.get.name.get == "int")
    assert(type2.get.name.get == "Class11")
    assert(type3.get.name.get == "Abstract class")

  }
  test("test parsers / generalization and specialization") {

    val IDS = List((eaFixture, "EAID_2D74DA5E_06F1_4e70_9B3C_46CF7F88331D"), (genMyModelFixture, "_LC2GrB7BEeSCHNJdddddKw"))
    IDS.foreach {
      id =>
        val f = id._1
        val ID = id._2
        info("XMI=" + f.vsHandler.XMI + " UML=" + f.vsHandler.UML + " Version=" + f.vsHandler.version)
        val node = f.r.resolve(ID)
        assert(node.isDefined)
        assert(node.get.isInstanceOf[XClass], "Entity is not instance of XClass")
        val cls = node.get.asInstanceOf[XClass]

        val gen = cls.generalizations
        assert(gen.size == 1)
        val xGen = gen.head
        assert(xGen.sourceId == ID)
        val parent = f.r.resolve(xGen.targetId)
        assert(parent.isDefined)
        assert(parent.get.isInstanceOf[XClass], "Parent entity is not instance of XClass")
        val parentCls = parent.get.asInstanceOf[XClass]
        assert(parentCls.name.isDefined)

        val real = cls.realizations
        assert(real.size == 1)
        val xReal = real.head
        assert(xReal.sourceId == ID)
        val parent2 = f.r.resolve(xReal.targetId)
        assert(parent2.isDefined)
        assert(parent2.get.isInstanceOf[XInterface], "Parent entity is not instance of XInterface")
        val parentInterf = parent2.get.asInstanceOf[XInterface]
        assert(parentInterf.name.isDefined)
    }
  }
  test("IDResolver") {

    fixtures.foreach {
      f =>

        info("XMI=" + f.vsHandler.XMI + " UML=" + f.vsHandler.UML + " Version=" + f.vsHandler.version)
        val numberOfEntities = f.model.allChildren.size + 1
        val ids = f.model.allChildren.map(c => c.id) :+ f.model.id
        val idsDistinct = ids.distinct.size
        val idsTotal = ids.size
        assert(idsTotal == idsDistinct, "Model contains duplicate elements: " + (idsTotal - idsDistinct))
        assert(numberOfEntities == f.r.idMap.size)

        assert(idsTotal > 20)
    }
  }
}