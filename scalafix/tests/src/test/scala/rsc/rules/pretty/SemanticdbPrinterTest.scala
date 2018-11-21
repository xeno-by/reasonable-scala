package rsc.rules.pretty

import metaconfig.Conf
import metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
import rsc.rules.RscCompatConfig
import rsc.rules.semantics._
import scalafix.internal.reflect.ClasspathOps
import scalafix.testkit._
import scala.meta._
import scalafix.TestHelper
import scalafix.internal.config.ScalafixConfig
import scalafix.internal.diff.DiffDisable
import scalafix.internal.v1.DocumentIndex
import scalafix.internal.v1.LazyValue

class SemanticdbPrinterTest extends SemanticRuleSuite {

  private val symtab = ClasspathOps
    .newSymbolTable(props.inputClasspath)
    .getOrElse { sys.error("Failed to load symbol table") }
  private val classLoader = ClasspathOps.toClassLoader(props.inputClasspath)
  private val emptyEnv = Env(Nil)
  private val rootScope = PackageScope(symtab, "_root_/")
  private val javaLangScope = ImporterScope(symtab, "java/lang/", List(Importee.Wildcard()))
  private val scalaScope = ImporterScope(symtab, "scala/", List(Importee.Wildcard()))
  private val predefScope = ImporterScope(symtab, "scala/Predef.", List(Importee.Wildcard()))
  private val predefEnv = predefScope :: scalaScope :: javaLangScope :: rootScope :: emptyEnv
  private val aTest = TestkitPath.fromProperties(props).head
  private val input: Input = aTest.toInput
  private val tree = input.parse[Source].get
  private val comment = SemanticRuleSuite.findTestkitComment(tree.tokens)
  private val syntax = comment.syntax.stripPrefix("/*").stripSuffix("*/")
  private val conf = Conf.parseString(aTest.testName, syntax).get
  private val scalafixConfig = conf.as[ScalafixConfig].get
  private val doc = TestHelper.syntacticDoc(
    tree.pos.input,
    LazyValue.now(tree),
    DiffDisable.empty,
    scalafixConfig
  )
  private val semanticDoc = TestHelper.semanticDoc(doc, aTest.semanticdbPath, classLoader, symtab)
  private val index = DocumentIndex(semanticDoc)
  private val printer = new SemanticdbPrinter(predefEnv, index, RscCompatConfig.default)
  import printer._

  //Testing is very light for now but this is enough to demonstrate calling syntax
  //and exercise the test harness
  test("dealias Predef types to base type") {
    assert(dealias("scala/Predef.String#").contains("java/lang/String#"))
  }

  test("symIsInScope returns false when symbols is not in scope") {
    assert(!symIsInScope("scala/collection/mutable/Map#", predefEnv))
  }

  test("symIsInScope returns true when symbols is in scope") {
    assert(symIsInScope("scala/collection/immutable/Map#", predefEnv))
  }

}
