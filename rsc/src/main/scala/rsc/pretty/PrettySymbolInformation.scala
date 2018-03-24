// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._

object PrettySymbolInformation {
  def str(p: Printer, info: SymbolInformation): Unit = {
    // TODO: Implement me.
  }

  def repl(p: Printer, info: SymbolInformation): Unit = {
    p.str(info.toProtoString)
  }
}
