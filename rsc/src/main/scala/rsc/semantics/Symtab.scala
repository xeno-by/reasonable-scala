// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import java.util.{HashMap, Map}
import scala.collection.mutable.UnrolledBuffer
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.SymbolOccurrence.{Role => r}
import rsc.lexis._
import rsc.pretty._
import rsc.syntax._
import rsc.util._

final class Symtab private extends Pretty {
  val _scopes: Map[Uid, Scope] = new HashMap[Uid, Scope]
  val _outlines: Map[Uid, Outline] = new HashMap[Uid, Outline]
  val _semanticdbs: Map[Input, s.TextDocument] =
    new HashMap[Input, s.TextDocument]

  object scopes {
    def apply(uid: Uid): Scope = {
      val scope = _scopes.get(uid)
      if (scope == null) {
        unreachable(uid)
      }
      scope
    }

    def update(uid: Uid, scope: Scope): Unit = {
      if (_scopes.containsKey(uid)) {
        unreachable(uid)
      }
      uid match {
        case NoUid => unreachable(scope)
        case other => _scopes.put(other, scope)
      }
    }
  }

  object outlines {
    def apply(uid: Uid): Outline = {
      val outline = _outlines.get(uid)
      if (outline == null) {
        unreachable(uid)
      }
      outline
    }

    def update(uid: Uid, outline: Outline): Unit = {
      if (_outlines.containsKey(uid)) {
        unreachable(uid)
      }
      uid match {
        case NoUid => unreachable(outline)
        case other => _outlines.put(uid, outline)
      }
    }
  }

  object semanticdbs {
    private var _nextId = 0
    private var localSymbols = new HashMap[Uid, String]()

    def apply(input: Input): s.TextDocument = {
      var document = _semanticdbs.get(input)
      if (document == null) {
        val filename = input.file.getName
        val occs = new UnrolledBuffer[s.SymbolOccurrence]()
        document = s.TextDocument(
          schema = s.Schema.SEMANTICDB3,
          uri = filename,
          language = Some(s.Language("ReasonableScala")),
          occurrences = occs
        )
        _semanticdbs.put(input, document)
      }
      document
    }

    object definitions {
      def update(id: Id, uid: Uid): Unit = {
        semanticdbs.occurrences.update(id, uid, isDefinition = true)
      }
    }

    object references {
      def update(id: Id, uid: Uid): Unit = {
        semanticdbs.occurrences.update(id, uid, isDefinition = false)
      }
    }

    object occurrences {
      def update(id: Id, uid: Uid, isDefinition: Boolean): Unit = {
        val document = apply(id.pos.input)
        document.occurrences match {
          case occs: UnrolledBuffer[s.SymbolOccurrence] =>
            val range = s.Range(
              id.pos.startLine,
              id.pos.startColumn,
              id.pos.endLine,
              id.pos.endColumn)
            val symbol = {
              if (uid.startsWith("_")) uid
              else {
                var symbol = localSymbols.get(uid)
                if (symbol == null) {
                  val id = _nextId
                  _nextId += 1
                  symbol = "local" + id
                  localSymbols.put(uid, symbol)
                }
                symbol
              }
            }
            val role = if (isDefinition) r.DEFINITION else r.REFERENCE
            val occ = s.SymbolOccurrence(Some(range), symbol, role)
            occs.append(occ)
          case other =>
            unreachable(other.toString)
        }
      }
    }
  }

  def printStr(p: Printer): Unit = {
    PrettySymtab.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettySymtab.repl(p, this)
  }
}

object Symtab {
  def apply(): Symtab = {
    new Symtab
  }
}
