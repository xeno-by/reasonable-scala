// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import java.util.HashMap
import rsc.pretty._
import rsc.semantics._
import rsc.util._

final class Symtab private extends Pretty {
  val _scopes = new HashMap[Symbol, Scope]
  val _infos = new HashMap[Symbol, SymbolInformation]

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

  object infos {
    def apply(sym: Symbol): SymbolInformation = {
      val info = _infos.get(sym)
      if (info == null) {
        unreachable(sym)
      }
      info
    }

    def update(sym: Symbol, info: SymbolInformation): Unit = {
      if (_infos.containsKey(sym)) {
        unreachable(sym)
      }
      sym match {
        case NoSymbol => unreachable(info)
        case other => _infos.put(sym, info)
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
