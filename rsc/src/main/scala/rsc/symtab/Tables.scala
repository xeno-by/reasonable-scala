// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.pretty._
import rsc.semantics._
import rsc.typecheck._
import rsc.util._

trait Tables {
  self: Symtab =>

  val _scopes = new Table[Scope]
  val _infos = new Table[SymbolInformation]

  class Table[T: Str: Repl] extends Pretty {
    val _storage = new HashMap[Symbol, T]

    private def load(sym: Symbol): T = {
      var payload = _storage.get(sym)
      if (payload == null) {
        loadFromClasspath(sym)
        payload = _storage.get(sym)
      }
      payload
    }

    def apply(sym: Symbol): T = {
      val payload = load(sym)
      if (payload == null) {
        unreachable(sym)
      }
      payload
    }

    def contains(sym: Symbol): Boolean = {
      val payload = load(sym)
      payload != null
    }

    def update(sym: Symbol, payload: T): Unit = {
      if (_storage.containsKey(sym)) {
        unreachable(sym)
      }
      sym match {
        case NoSymbol => unreachable(payload)
        case other => _storage.put(other, payload)
      }
    }

    def printStr(p: Printer): Unit = {
      PrettySymtab.str(p, this)
    }

    def printRepl(p: Printer): Unit = {
      PrettySymtab.repl(p, this)
    }
  }
}
