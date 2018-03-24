// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._
import rsc.semantics.SymbolInformation._
import rsc.semantics.SymbolInformation.{Kind => k}
import rsc.semantics.SymbolInformation.{Property => prop}

object PrettySymbolInformation {
  def str(p: Printer, x: SymbolInformation): Unit = {
    if (x.symbol.nonEmpty) {
      p.str(x.symbol)
    } else {
      p.str("<?>")
    }
    p.str(" => ")
    p.rep(x.annotations, " ", " ")(ann => p.str(ann))
    p.opt(x.accessibility, " ")(acc => p.str(acc))
    def has(prop: Property) = (x.properties & prop.value) != 0
    if (has(prop.ABSTRACT)) p.str("abstract ")
    if (has(prop.FINAL)) p.str("final ")
    if (has(prop.SEALED)) p.str("sealed ")
    if (has(prop.IMPLICIT)) p.str("implicit ")
    if (has(prop.LAZY)) p.str("lazy ")
    if (has(prop.CASE)) p.str("case ")
    if (has(prop.COVARIANT)) p.str("covariant ")
    if (has(prop.CONTRAVARIANT)) p.str("contravariant ")
    if (has(prop.VALPARAM)) p.str("valparam ")
    if (has(prop.VARPARAM)) p.str("varparam ")
    x.kind match {
      case k.VAL => p.str("val ")
      case k.VAR => p.str("var ")
      case k.DEF => p.str("def ")
      case k.GETTER => p.str("getter ")
      case k.SETTER => p.str("setter ")
      case k.PRIMARY_CONSTRUCTOR => p.str("primaryctor ")
      case k.SECONDARY_CONSTRUCTOR => p.str("secondaryctor ")
      case k.MACRO => p.str("macro ")
      case k.TYPE => p.str("type ")
      case k.PARAMETER => p.str("param ")
      case k.SELF_PARAMETER => p.str("selfparam ")
      case k.TYPE_PARAMETER => p.str("typeparam ")
      case k.OBJECT => p.str("object ")
      case k.PACKAGE => p.str("package ")
      case k.PACKAGE_OBJECT => p.str("package object ")
      case k.CLASS => p.str("class ")
      case k.TRAIT => p.str("trait ")
      case k.UNKNOWN_KIND | Kind.Unrecognized(_) => ()
    }
    if (x.name.nonEmpty) {
      p.str(x.name)
    } else {
      p.str("<?>")
    }
    x.kind match {
      case k.VAL | k.VAR | k.DEF | k.GETTER | k.SETTER | k.PRIMARY_CONSTRUCTOR |
          k.SECONDARY_CONSTRUCTOR | k.MACRO | k.TYPE | k.PARAMETER |
          k.SELF_PARAMETER | k.TYPE_PARAMETER =>
        x.tpe match {
          case Some(tpe) =>
            p.str(": ")
            p.str(tpe)
          case None =>
            p.str(": <?>")
        }
      case k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT =>
        x.tpe.flatMap(_.classInfoType) match {
          case Some(ClassInfoType(tparams, parents, decls)) =>
            // TODO: Implement me.
            p.rep("[", tparams, ", ", "]")(sym => p.str("<" + sym + ">"))
            p.rep("extends ", parents, " with ")(parent => p.str(parent))
            p.str(s" {+${decls.length} decls}")
          case None =>
            p.str(": <?>")
        }
      case k.PACKAGE | k.UNKNOWN_KIND | Kind.Unrecognized(_) =>
        ()
    }
  }

  def repl(p: Printer, x: SymbolInformation): Unit = {
    // TODO: Implement me.
    p.str(x.toProtoString)
  }
}
