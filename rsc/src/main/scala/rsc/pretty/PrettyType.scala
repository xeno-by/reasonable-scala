// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._
import rsc.util._

object PrettyType {
  def str(p: Printer, tpe: Type): Unit = {
    tpe match {
      case NoType =>
        p.str("ø")
      case SimpleType(sym, targs) =>
        p.str("<" + sym + ">")
        if (targs.nonEmpty) {
          p.str("[")
          p.rep(targs, ", ")(targ => p.str(targ.toString))
          p.str("]")
        }
      case FunctionType(tparams, params, ret) =>
        if (tparams.nonEmpty) {
          p.str("[")
          p.rep(tparams, ", ")(sym => p.str("<" + sym + ">"))
          p.str("]")
        }
        p.str("(")
        p.rep(params, ", ")(sym => p.str("<" + sym + ">"))
        p.str(")")
        p.str(":")
        p.str(ret)
      case _ =>
        unreachable(tpe.toProtoString)
    }
  }

  def repl(p: Printer, tpe: Type): Unit = {
    tpe match {
      case NoType =>
        p.str("NoType")
      case SimpleType(sym, targs) =>
        p.str("SimpleType(")
        p.repl(sym)
        p.str(", ")
        p.repl(targs)
        p.str(")")
      case FunctionType(tparams, params, ret) =>
        p.str("FunctionType(")
        p.repl(tparams)
        p.str(", ")
        p.repl(params)
        p.str(", ")
        p.repl(ret)
        p.str(")")
      case _ =>
        unreachable(tpe.toProtoString)
    }
  }
}
