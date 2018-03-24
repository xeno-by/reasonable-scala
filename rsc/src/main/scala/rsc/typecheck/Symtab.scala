// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import java.util.{HashMap, Map}
import rsc.pretty._
import rsc.semantics._
import rsc.syntax._
import rsc.util._

final class Symtab private extends Pretty {
  val _scopes: Map[Symbol, Scope] = new HashMap[Symbol, Scope]
  val _outlines: Map[Symbol, Outline] = new HashMap[Symbol, Outline]

  object scopes {
    def apply(sym: Symbol): Scope = {
      val scope = _scopes.get(sym)
      if (scope == null) {
        unreachable(sym)
      }
      scope
    }

    def update(sym: Symbol, scope: Scope): Unit = {
      if (_scopes.containsKey(sym)) {
        unreachable(sym)
      }
      sym match {
        case NoSymbol => unreachable(scope)
        case other => _scopes.put(other, scope)
      }
    }
  }

  object outlines {
    def apply(sym: Symbol): Outline = {
      val outline = _outlines.get(sym)
      if (outline == null) {
        unreachable(sym)
      }
      outline
    }

    def update(sym: Symbol, outline: Outline): Unit = {
      if (_outlines.containsKey(sym)) {
        unreachable(sym)
      }
      sym match {
        case NoSymbol => unreachable(outline)
        case other => _outlines.put(sym, outline)
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
