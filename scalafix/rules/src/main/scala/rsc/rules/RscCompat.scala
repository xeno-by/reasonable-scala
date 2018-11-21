// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalacenter/scalafix.
package rsc.rules

import java.io._
import metaconfig._
import rsc.rules.pretty._
import rsc.rules.semantics._
import rsc.rules.syntax._
import scala.meta._
import scala.meta.contrib._
import scala.meta.internal.{semanticdb => s}
import scalafix.internal.v1._
import scalafix.lint.Diagnostic
import scalafix.patch.Patch
import scalafix.v1.{Configuration, Rule}
import scalafix.util.TokenOps
import scalafix.v1.SemanticDocument
import scalafix.v1._

case class RscCompat(config: RscCompatConfig)
    extends SemanticdbRule( "RscCompat") {
  def this() = this(RscCompatConfig.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    config.conf
      .getOrElse("rscCompat", "RscCompat")(RscCompatConfig.default)
      .map(RscCompat(_))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    val targets = collectRewriteTargets(doc)
    targets.map(ascribeReturnType(doc, _)).asPatch
  }

  case class RewriteTarget(
      env: Env,
      before: Token,
      name: Name,
      after: Token,
      body: Term,
      parens: Boolean)

  def collectRewriteTargets(doc: SemanticDocument): List[RewriteTarget] = {
    implicit val impDoc = doc
    val buf = List.newBuilder[RewriteTarget]
    def loop(env: Env, tree: Tree): Env = {
      tree match {
        case Source(stats) =>
          val rootScope = PackageScope(index.symbols, "_root_/")
          val javaLangScope = ImporterScope(index.symbols, "java/lang/", List(Importee.Wildcard()))
          val scalaScope = ImporterScope(index.symbols, "scala/", List(Importee.Wildcard()))
          val predefScope = ImporterScope(index.symbols, "scala/Predef.", List(Importee.Wildcard()))
          val env1 = predefScope :: scalaScope :: javaLangScope :: rootScope :: env
          stats.foldLeft(env1)(loop)
        case Import(importers) =>
          importers.foldLeft(env)(loop)
        case Importer(ref, importees) =>
          return ImporterScope(index.symbols, ref.name.symbol.value, importees) :: env
        case Pkg(ref, stats) =>
          val env1 = PackageScope(index.symbols, ref.name.symbol.value) :: env
          stats.foldLeft(env1)(loop)
        case Pkg.Object(_, name, templ) =>
          val env1 = TemplateScope(index.symbols, name.symbol.value) :: env
          loop(env1, templ)
        case defn @ Defn.Class(_, name, _, _, templ) if defn.isVisible =>
          val env1 = TemplateScope(index.symbols, name.symbol.value) :: env
          loop(env1, templ)
        case defn @ Defn.Trait(_, name, _, _, templ) if defn.isVisible =>
          val env1 = TemplateScope(index.symbols, name.symbol.value) :: env
          loop(env1, templ)
        case defn @ Defn.Object(_, name, templ) if defn.isVisible =>
          val env1 = TemplateScope(index.symbols, name.symbol.value) :: env
          loop(env1, templ)
        case Template(early, _, _, stats) =>
          (early ++ stats).foldLeft(env)(loop)
        case defn @ InferredDefnField(name, body) if defn.isVisible =>
          val before = name.tokens.head
          val after = name.tokens.last
          buf += RewriteTarget(env, before, name, after, body, parens = false)
        case defn @ InferredDefnPat(fnames, pnames, body) if defn.isVisible =>
          if (fnames.nonEmpty) {
            val name = fnames.head
            val before = name.tokens.head
            val after = {
              val start = name.tokens.head
              val end = body.tokens.head
              val slice = doc.tokenList.slice(start, end)
              slice.reverse
                .find(x => !x.is[Token.Equals] && !x.is[Trivia])
                .get
            }
            buf += RewriteTarget(env, before, name, after, body, parens = false)
          }
          pnames.foreach { name =>
            val before = name.tokens.head
            val after = name.tokens.last
            // FIXME: https://github.com/twitter/rsc/issues/142
            buf += RewriteTarget(env, before, name, after, body, parens = true)
          }
        case defn @ InferredDefnDef(name, body) if defn.isVisible =>
          val before = name.tokens.head
          val after = {
            val start = name.tokens.head
            val end = body.tokens.head
            val slice = doc.tokenList.slice(start, end)
            slice.reverse
              .find(x => !x.is[Token.Equals] && !x.is[Trivia])
              .get
          }
          buf += RewriteTarget(env, before, name, after, body, parens = false)
        case _ =>
          ()
      }
      env
    }
    loop(Env(Nil), doc.tree)
    buf.result
  }

  def ascribeReturnType(doc: SemanticDocument, target: RewriteTarget): Patch = {
    implicit val impDoc = doc
    try {
      val returnTypeString = {
        val symbol = target.name.symbol.value
        config.hardcoded.get(symbol) match {
          case Some(returnTypeString) =>
            returnTypeString
          case _ if index.symbols.get(symbol).isDefined =>
            //FIXME, ignore symbols that are not found as pattern syms are returning as localn
            //with no symbo on lookup due to https://github.com/scalameta/scalameta/issues/1725
            val info = index.symbols(symbol)
            target.body match {
              case Term.ApplyType(Term.Name("implicitly"), _) if info.isImplicit =>
                return Patch.empty
              case Term.ApplyType(Term.Select(Term.Name("Bijection"), Term.Name("connect")), _)
                  if info.isImplicit =>
                return Patch.empty
              case _ =>
                val returnType = info.signature match {
                  case s.MethodSignature(_, _, _: s.ConstantType) =>
                    return Patch.empty
                  case s.MethodSignature(_, _, returnType) =>
                    returnType
                  case s.ValueSignature(tpe) =>
                    // FIXME: https://github.com/scalameta/scalameta/issues/1725
                    tpe
                  case other =>
                    val details = other.asMessage.toProtoString
                    sys.error(s"unsupported outline: $details")
                }
                val printer = new SemanticdbPrinter(target.env, index, config)
                printer.pprint(returnType)
                printer.toString
            }
          case _ =>
            //FIXME: Do nothing until https://github.com/scalameta/scalameta/issues/1725 is resolved
            return Patch.empty
        }
      }
      if (returnTypeString.nonEmpty) {
        val before = {
          val lparenOpt = if (target.parens) "(" else ""
          Patch.addLeft(target.before, lparenOpt)
        }
        val after = {
          val whitespaceOpt = {
            if (TokenOps.needsLeadingSpaceBeforeColon(target.after)) " "
            else ""
          }
          val ascription = s": $returnTypeString"
          val rparenOpt = if (target.parens) ")" else ""
          Patch.addRight(target.after, whitespaceOpt + ascription + rparenOpt)
        }
        before + after
      } else {
        Patch.empty
      }
    } catch {
      case ex: Throwable =>
        val sw = new java.io.StringWriter()
        ex.printStackTrace(new PrintWriter(sw))
        Patch.lint(Diagnostic("RscCompat", sw.toString, target.name.pos))
    }
  }
}
