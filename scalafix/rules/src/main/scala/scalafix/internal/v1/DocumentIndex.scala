// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v1

import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scalafix.v1.SemanticDocument

case class DocumentIndex(index: InternalSemanticDoc) {
  lazy val doc: s.TextDocument = {
    index.textDocument
  }

  lazy val input: Input = {
    Input.VirtualFile(doc.uri, doc.text)
  }

  def substring(range: Option[s.Range]): Option[String] = {
    range.flatMap { range =>
      if (doc.text.nonEmpty) {
        val pos = Position.Range(
          input,
          range.startLine,
          range.startCharacter,
          range.endLine,
          range.endCharacter)
        Some(pos.text)
      } else {
        None
      }
    }
  }

  lazy val symbols: DocumentSymbols = {
    DocumentSymbols(index.symtab)
  }

  lazy val synthetics: Map[s.Range, s.Synthetic] = {
    doc.synthetics.map(synth => synth.range.get -> synth).toMap
  }
}

object DocumentIndex {
  def apply(semanticDoc: SemanticDocument): DocumentIndex =
    DocumentIndex(semanticDoc.internal)
}
