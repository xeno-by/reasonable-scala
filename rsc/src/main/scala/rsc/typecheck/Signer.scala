// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.report._
import rsc.semantics._
import rsc.semantics.MethodType._
import rsc.semantics.SymbolInformation.{Kind => k}
import rsc.semantics.Type.Tag._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Signer private (settings: Settings, reporter: Reporter) {
  def apply(env: Env, outline: Outline): SymbolInformation = {
    // TODO: Implement me.
    outline match {
      case outline: DefnClass =>
        val tparams = outline.tparams.map(_.id.sym)
        val tpe = ClassInfoType(tparams, Nil, Nil)
        SymbolInformation(
          kind = k.CLASS,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = CLASS_INFO_TYPE, classInfoType = Some(tpe))))
      case outline: DefnDef =>
        val tparams = outline.tparams.map(_.id.sym)
        val paramss = List(ParameterList(outline.params.map(_.id.sym)))
        val ret = outline.ret.tpe
        val tpe = MethodType(tparams, paramss, Some(ret))
        SymbolInformation(
          kind = k.DEF,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = METHOD_TYPE, methodType = Some(tpe))))
      case outline: DefnField =>
        val isVal = outline.mods.exists(_.isInstanceOf[ModVal])
        SymbolInformation(
          kind = if (isVal) k.VAL else k.VAR,
          symbol = outline.id.sym,
          tpe = Some(outline.tpt.tpe))
      case outline: DefnObject =>
        val tpe = ClassInfoType(Nil, Nil, Nil)
        SymbolInformation(
          kind = k.OBJECT,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = CLASS_INFO_TYPE, classInfoType = Some(tpe))))
      case outline: DefnPackage =>
        SymbolInformation(kind = k.PACKAGE, symbol = outline.id.sym)
      case outline: DefnTrait =>
        val tparams = outline.tparams.map(_.id.sym)
        val tpe = ClassInfoType(tparams, Nil, Nil)
        SymbolInformation(
          kind = k.TRAIT,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = CLASS_INFO_TYPE, classInfoType = Some(tpe))))
      case outline: DefnType =>
        val tparams = outline.tparams.map(_.id.sym)
        val lo = Some(outline.tpt.tpe)
        val hi = Some(outline.tpt.tpe)
        val tpe = TypeType(tparams, lo, hi)
        SymbolInformation(
          kind = k.TYPE,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = TYPE_TYPE, typeType = Some(tpe))))
      case outline: PatVar =>
        val sym = {
          outline.id match {
            case AnonId() => ""
            case id: NamedId => id.sym
          }
        }
        SymbolInformation(kind = k.VAL, symbol = sym, tpe = Some(outline.tpe))
      case outline: PrimaryCtor =>
        val paramss = List(ParameterList(outline.params.map(_.id.sym)))
        val tpe = MethodType(Nil, paramss, None)
        SymbolInformation(
          kind = k.PRIMARY_CONSTRUCTOR,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = METHOD_TYPE, methodType = Some(tpe))))
      case outline: TermParam =>
        SymbolInformation(
          kind = k.PARAMETER,
          symbol = outline.id.sym,
          tpe = Some(outline.tpt.tpe))
      case outline: TypeParam =>
        val lo = outline.lo.tpe
        val hi = outline.hi.tpe
        val tpe = TypeType(Nil, Some(lo), Some(hi))
        SymbolInformation(
          kind = k.TYPE_PARAMETER,
          symbol = outline.id.sym,
          tpe = Some(Type(tag = TYPE_TYPE, typeType = Some(tpe))))
    }
  }

  private implicit class SignerTptOps(tpt: Tpt) {
    def tpe: Type = {
      tpt match {
        case TptApply(fun: TptPath, targs) =>
          if (fun.id.sym == NoSymbol) unreachable(fun)
          else SimpleType(fun.id.sym, targs.map(_.tpe))
        case _: TptApply =>
          unreachable(tpt)
        case tpt: TptPath =>
          if (tpt.id.sym == NoSymbol) unreachable(tpt.id)
          else SimpleType(tpt.id.sym, Nil)
      }
    }
  }
}

object Signer {
  def apply(settings: Settings, reporter: Reporter): Signer = {
    new Signer(settings, reporter)
  }
}
