// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._
import rsc.semantics.MethodType._
import rsc.semantics.Type.Tag._
import rsc.util._

trait Types {
  implicit object TypeStr extends Str[Type] {
    def apply(p: Printer, tpe: Type): Unit = {
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
  }

  implicit object TypeRepl extends Repl[Type] {
    def apply(p: Printer, tpe: Type): Unit = {
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

  val NoType = null

  object SimpleType {
    def apply(sym: Symbol, targs: List[Type]): Type = {
      Type(tag = TYPE_REF, typeRef = Some(TypeRef(None, sym, targs)))
    }

    def unapply(tpe: Type): Option[(Symbol, List[Type])] = {
      tpe.tag match {
        case TYPE_REF =>
          val Some(TypeRef(pre, sym, targs)) = tpe.typeRef
          if (pre != None) unreachable(tpe)
          Some((sym, targs.toList))
        case _ =>
          None
      }
    }
  }

  // NOTE: This object has previously been called MethodType.
  // I've recently renamed it to FunctionType.
  //
  // I know that the new name is incorrect, because function types
  // don't have type parameters, and because function types are defined
  // differently in SLS.
  //
  // However, the new name will help avoid naming conflicts in the upcoming
  // refactoring to use SemanticDB, so I think it's worth it for the
  // transition period.

  object FunctionType {
    def apply(tparams: List[Symbol], params: List[Symbol], ret: Type): Type = {
      val stparams = tparams
      val sparamss = List(ParameterList(params))
      val sret = Some(ret)
      val smethodType = Some(MethodType(stparams, sparamss, sret))
      Type(tag = METHOD_TYPE, methodType = smethodType)
    }

    def unapply(tpe: Type): Option[(List[Symbol], List[Symbol], Type)] = {
      tpe.tag match {
        case METHOD_TYPE =>
          val Some(MethodType(stparams, sparamss, sret)) = tpe.methodType
          val tparams = stparams.toList
          val params = {
            sparamss match {
              case List() => Nil
              case List(ParameterList(params)) => params.toList
              case _ => unreachable(tpe)
            }
          }
          val Some(ret) = sret
          Some((tparams, params, ret))
        case _ =>
          None
      }
    }
  }
}
