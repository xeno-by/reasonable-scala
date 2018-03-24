// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import rsc.pretty._

package object semantics extends Symbols with Types {
  implicit val typeStr = Str[Type](PrettyType.str)
  implicit val typeRepl = Repl[Type](PrettyType.repl)

  implicit val infoStr = Str[SymbolInformation](PrettySymbolInformation.str)
  implicit val infoRepl = Repl[SymbolInformation](PrettySymbolInformation.repl)
}
