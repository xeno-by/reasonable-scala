// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._

sealed trait Type extends Pretty with Product {
  def printStr(p: Printer): Unit = PrettyType.str(p, this)
  def printRepl(p: Printer): Unit = PrettyType.repl(p, this)
}

final case object NoType extends Type

final case class SimpleType(sym: Symbol, targs: List[SimpleType]) extends Type

// NOTE: This class has previously been called MethodType.
// I've recently renamed it to FunctionType.
//
// I know that the new name is incorrect, because function types
// don't have type parameters, and because function types are defined
// differently in SLS.
//
// However, the new name will help avoid naming conflicts in the upcoming
// refactoring to use SemanticDB, so I think it's worth it for the
// transition period.
final case class FunctionType(
    tparams: List[Symbol],
    params: List[Symbol],
    ret: SimpleType)
    extends Type
