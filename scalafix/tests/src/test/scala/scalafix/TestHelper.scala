package scalafix
import scala.meta.Tree
import scala.meta.inputs.Input
import scala.meta.internal.symtab.SymbolTable
import scala.meta.io.RelativePath
import scalafix.internal.config.ScalafixConfig
import scalafix.internal.diff.DiffDisable
import scalafix.internal.v1.LazyValue
import scalafix.v1.{SemanticDocument, SyntacticDocument}

object TestHelper {
  def semanticDoc(
    doc: SyntacticDocument,
    path: RelativePath,
    cl: ClassLoader,
    symtab: SymbolTable)
  : SemanticDocument = SemanticDocument.fromPath(doc, path, cl, symtab)

  def syntacticDoc(
    input: Input,
    tree: LazyValue[Tree],
    diffDisable: DiffDisable,
    config: ScalafixConfig): SyntacticDocument =
    v1.SyntacticDocument(input, tree, diffDisable, config)
}
