// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.lexis._
import rsc.report._
import rsc.semantics._
import rsc.semantics.SymbolInformation.{Kind => k}
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Typechecker private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab) {
  def apply(env: Env, tree: Typeable): Type = {
    tree match {
      case tree: Init => init(env, tree)
      case tree: TermApply => termApply(env, tree)
      case tree: TermApplyInfix => termApplyInfix(env, tree)
      case tree: TermApplyPrefix => termApplyPrefix(env, tree)
      case tree: TermApplyType => termApplyType(env, tree)
      case tree: TermAssign => termAssign(env, tree)
      case tree: TermBlock => termBlock(env, tree)
      case tree: TermFunction => termFunction(env, tree)
      case tree: TermId => termId(env, tree)
      case tree: TermIf => termIf(env, tree)
      case tree: TermLit => termLit(env, tree)
      case tree: TermMatch => termMatch(env, tree)
      case tree: TermNew => termNew(env, tree)
      case tree: TermReturn => termReturn(env, tree)
      case tree: TermSelect => termSelect(env, tree)
      case tree: TermSuper => termSuper(env, tree)
      case tree: TermThis => termThis(env, tree)
      case tree: TermThrow => termThrow(env, tree)
      case tree: TermWhile => termWhile(env, tree)
      case tree: TptApply => tptApply(env, tree)
      case tree: TptId => tptId(env, tree)
      case tree: TptSelect => tptSelect(env, tree)
      case _ => unreachable(tree)
    }
  }

  private def init(env: Env, tree: Init): Type = {
    tree.args.foreach(apply(env, _))
    apply(env, tree.tpt)
  }

  private def termApply(env: Env, tree: TermApply): Type = {
    val funTpe = apply(env, tree.fun)
    tree.args.foreach(apply(env, _))
    funTpe match {
      case NoType =>
        NoType
      case FunctionType(Nil, _, ret) =>
        ret
      case FunctionType(other, _, _) =>
        unsupported("type inference")
      case other =>
        val id1 = TermId("apply").withPos(tree.fun.pos.end, tree.fun)
        val select1 = TermSelect(tree.fun, id1).withPos(tree.fun)
        val tree1 = TermApply(select1, tree.args).withPos(tree)
        apply(env, tree1)
    }
  }

  private def termApplyInfix(env: Env, tree: TermApplyInfix): Type = {
    if (tree.op.value.isLeftAssoc) {
      val select = TermSelect(tree.lhs, tree.op).withPos(tree.lhs, tree.op)
      val applyType = {
        if (tree.targs.isEmpty) select
        else TermApplyType(select, tree.targs).withPos(select, tree.targs.last)
      }
      val tree1 = TermApply(applyType, List(tree.rhs)).withPos(tree)
      apply(env, tree1)
    } else {
      unreachable(tree)
    }
  }

  private def termApplyPrefix(env: Env, tree: TermApplyPrefix): Type = {
    val arg1 = TermSelect(tree.arg, tree.op).withPos(tree)
    val tree1 = TermApply(arg1, Nil).withPos(tree)
    apply(env, tree1)
  }

  private def termApplyType(env: Env, tree: TermApplyType): Type = {
    val funTpe = apply(env, tree.fun)
    val targs = tree.targs.map { tpt =>
      apply(env, tpt) match {
        case NoType => return NoType
        case tpe @ SimpleType(_, _) => tpe
        case other => unreachable(other)
      }
    }
    funTpe match {
      case NoType =>
        NoType
      case FunctionType(tparams, paramss, ret) =>
        funTpe.subst(tparams, targs)
      case other =>
        val id1 = TermId("apply").withPos(tree.fun.pos.end, tree.fun)
        val select1 = TermSelect(tree.fun, id1).withPos(tree.fun)
        val tree1 = TermApplyType(select1, tree.targs).withPos(tree)
        apply(env, tree1)
    }
  }

  private def termAssign(env: Env, tree: TermAssign): Type = {
    tree.lhs match {
      case TermApply(fun, args) =>
        val id1 = TermId("update").withPos(tree.lhs.pos.end, tree.lhs)
        val select1 = TermSelect(fun, id1).withPos(tree.lhs)
        val tree1 = TermApply(select1, args :+ tree.rhs).withPos(tree)
        apply(env, tree1)
      case other =>
        apply(env, other)
        apply(env, tree.rhs)
    }
  }

  private def termBlock(env: Env, tree: TermBlock): Type = {
    tree.stats match {
      case stats :+ (term: Term) =>
        val outlines = List.newBuilder[Outline]
        val tpts = List.newBuilder[Tpt]
        val terms = List.newBuilder[Term]
        val scope = FlatScope("block")
        stats.foreach {
          case stat @ DefnField(_, id, tpt, Some(rhs)) =>
            val sym = scope.sym + id.name.str
            scope.enter(id.name, sym) match {
              case NoSymbol =>
                id.sym = sym
                outlines += stat
              case existingSym =>
                reporter.append(DoubleDef(existingSym, stat, Todo(), symtab))
                return NoType
            }
            tpts += tpt
            terms += rhs
          case term: Term =>
            terms += term
          case other =>
            unreachable(other)
        }
        scope.succeed()
        val env1 = scope :: env
        tpts.result.foreach(apply(env1, _))
        val signer = Signer(settings, reporter, symtab)
        outlines.result.foreach { outline =>
          val info = signer.apply(env1, outline)
          symtab.infos(outline.id.sym) = info
        }
        terms.result.foreach(apply(env1, _))
        apply(env1, term)
      case Nil =>
        SimpleType("_root_.scala.Unit#", Nil)
      case other =>
        unreachable(other)
    }
  }

  private def termFunction(env: Env, tree: TermFunction): Type = {
    val outlines = List.newBuilder[Outline]
    val tpts = List.newBuilder[Tpt]
    val scope = FlatScope("lambda")
    tree.params.foreach {
      case param @ TermParam(_, id, tpt) =>
        val sym = scope.sym + id.name.str
        scope.enter(id.name, sym) match {
          case NoSymbol =>
            id.sym = sym
            outlines += param
          case existingSym =>
            reporter.append(DoubleDef(existingSym, param, Todo(), symtab))
            return NoType
        }
        tpts += tpt
    }
    scope.succeed()
    val env1 = scope :: env
    tpts.result.foreach(apply(env, _))
    val signer = Signer(settings, reporter, symtab)
    outlines.result.foreach { outline =>
      val info = signer.apply(env, outline)
      symtab.infos(outline.id.sym) = info
    }
    apply(env1, tree.body)
  }

  private def termId(env: Env, tree: TermId): Type = {
    env.lookup(tree.name) match {
      case NoSymbol =>
        reporter.append(UnboundId(tree))
        NoType
      case sym =>
        tree.sym = sym
        sym.tpe
    }
  }

  private def termIf(env: Env, tree: TermIf): Type = {
    apply(env, tree.cond)
    tree.elsep match {
      case Some(elsep) =>
        val thenTpe = apply(env, tree.thenp)
        val elseTpe = apply(env, elsep)
        lub(List(thenTpe, elseTpe))
      case None =>
        apply(env, tree.thenp)
        SimpleType("_root_.scala.Unit#", Nil)
    }
  }

  private def termLit(env: Env, tree: TermLit): Type = {
    tree.value match {
      case _: Unit => SimpleType("_root_.scala.Unit#", Nil)
      case _: Boolean => SimpleType("_root_.scala.Boolean#", Nil)
      case _: Byte => SimpleType("_root_.scala.Byte#", Nil)
      case _: Short => SimpleType("_root_.scala.Short#", Nil)
      case _: Char => SimpleType("_root_.scala.Char#", Nil)
      case _: Int => SimpleType("_root_.scala.Int#", Nil)
      case _: Float => SimpleType("_root_.scala.Float#", Nil)
      case _: Long => SimpleType("_root_.scala.Long#", Nil)
      case _: Double => SimpleType("_root_.scala.Double#", Nil)
      case _: String => SimpleType("_root_.java.lang.String#", Nil)
      case null => SimpleType("_root_.scala.AnyRef#", Nil)
      case other => unreachable(other.getClass.toString)
    }
  }

  private def termMatch(env: Env, tree: TermMatch): Type = {
    val termTpe = apply(env, tree.term)
    val caseTpes = List.newBuilder[Type]
    tree.cases.foreach {
      case caseDef @ Case(pat, cond, stats) =>
        val outlines = List.newBuilder[Outline]
        val scope = FlatScope("case")
        def loop(pat: Pat): Unit = {
          pat match {
            case pat: PatAlternative =>
              pat.pats.foreach(loop)
            case pat: PatId =>
              val tree1 = TermId(pat.value).withPos(pat)
              apply(env, tree1)
              pat.sym = tree1.sym
            case pat: PatLit =>
              ()
            case pat: PatSelect =>
              val tree1 = TermSelect(pat.qual, pat.id).withPos(pat)
              apply(env, tree1)
            case pat @ PatVar(id: NamedId, tpt) =>
              tpt match {
                case Some(tpt) =>
                  val sym = scope.sym + id.name.str
                  scope.enter(id.name, sym) match {
                    case NoSymbol =>
                      id.sym = sym
                      outlines += pat
                      pat.tpe = apply(env, tpt)
                    case existingSym =>
                      val message = DoubleDef(existingSym, pat, Todo(), symtab)
                      reporter.append(message)
                  }
                case None =>
                  unsupported("type inference")
              }
            case PatVar(AnonId(), _) =>
              ()
            case pat =>
              unsupported("advanced patterns")
          }
        }
        loop(pat)
        scope.succeed()
        val env1 = scope :: env
        cond.foreach(apply(env1, _))
        val signer = Signer(settings, reporter, symtab)
        outlines.result.foreach { outline =>
          val info = signer.apply(env, outline)
          symtab.infos(outline.id.sym) = info
        }
        val stats1 = TermBlock(stats).withPos(tree)
        caseTpes += apply(env1, stats1)
    }
    lub(caseTpes.result)
  }

  private def termNew(env: Env, tree: TermNew): Type = {
    apply(env, tree._init)
  }

  private def termReturn(env: Env, tree: TermReturn): Type = {
    tree.term.foreach(apply(env, _))
    SimpleType("_root_.scala.Nothing#", Nil)
  }

  // NOTE: termSelect contains an ad hoc informally-specified bug-ridden
  // slow implementation of asSeenFrom. It's so bad that we even had to
  // add a stub method Stack.get in Parser.scala to get things going.
  // However, this allowed us to typecheck re2s, so we're going to keep
  // this monstrosity alive for the time being.
  private def termSelect(env: Env, tree: TermSelect): Type = {
    val qualTpe = apply(env, tree.qual)
    qualTpe match {
      case NoType =>
        NoType
      case qualTpe @ FunctionType(_, _, _) =>
        reporter.append(NonValue(tree.qual, qualTpe))
        NoType
      case qualTpe @ SimpleType(_, _) =>
        def lookup(qualScopeSym: Symbol): Type = {
          val qualScope = symtab.scopes(qualScopeSym)
          qualScope.lookup(tree.id.name) match {
            case NoSymbol =>
              if (tree.id.value.isOpAssignment) {
                val value1 = tree.id.value.stripSuffix("=")
                val id1 = TermId(value1).withPos(tree.id)
                val tree1 = TermSelect(tree.qual, id1).withPos(tree)
                apply(env, tree1)
              } else {
                reporter.append(UnboundMember(qualScopeSym, tree.id))
                NoType
              }
            case sym =>
              tree.id.sym = sym
              sym.tpe
          }
        }
        def loop(qualTpe: Type): Type = {
          qualTpe match {
            case NoType =>
              NoType
            case FunctionType(_, _, _) =>
              unreachable(qualTpe)
            case SimpleType(qualSym, targs) =>
              val info = symtab.infos(qualSym)
              info.kind match {
                case k.PACKAGE =>
                  lookup(info.symbol)
                case k.CLASS | k.TRAIT | k.OBJECT =>
                  val ClassInfoType(tparams, _, _) =
                    info.tpe.get.classInfoType.get
                  lookup(info.symbol).subst(tparams, targs)
                case k.TYPE | k.TYPE_PARAMETER =>
                  val TypeType(tparams, _, Some(hi)) = info.tpe.get.typeType.get
                  loop(hi).subst(tparams, targs)
                case _ =>
                  unreachable(info)
              }
          }
        }
        loop(qualTpe)
    }
  }

  private def termSuper(env: Env, tree: TermSuper): Type = {
    env.lookupThis(tree.qual.nameopt) match {
      case NoSymbol =>
        reporter.append(UnboundId(tree.qual))
        NoType
      case qualSym =>
        tree.qual.sym = qualSym
        val env1 = Env(symtab.scopes(qualSym))
        env1.lookupSuper(tree.mix.nameopt) match {
          case NoSymbol =>
            reporter.append(UnboundId(tree.mix))
            NoType
          case mixSym =>
            tree.mix.sym = mixSym
            val info = symtab.infos(mixSym)
            info.kind match {
              case k.CLASS | k.TRAIT | k.OBJECT =>
                val ClassInfoType(tparams, _, _) =
                  info.tpe.get.classInfoType.get
                val targs = tparams.map(SimpleType(_, Nil)).toList
                SimpleType(info.symbol, targs)
              case _ =>
                unreachable(info)
            }
        }
    }
  }

  private def termThis(env: Env, tree: TermThis): Type = {
    env.lookupThis(tree.qual.nameopt) match {
      case NoSymbol =>
        reporter.append(UnboundId(tree.id))
        NoType
      case qualSym =>
        tree.qual.sym = qualSym
        val info = symtab.infos(qualSym)
        info.kind match {
          case k.CLASS | k.TRAIT | k.OBJECT =>
            val ClassInfoType(tparams, _, _) = info.tpe.get.classInfoType.get
            val targs = tparams.map(SimpleType(_, Nil)).toList
            SimpleType(info.symbol, targs)
          case _ =>
            unreachable(info)
        }
    }
  }

  private def termThrow(env: Env, tree: TermThrow): Type = {
    apply(env, tree.term)
    SimpleType("_root_.scala.Nothing#", Nil)
  }

  private def termWhile(env: Env, tree: TermWhile): Type = {
    apply(env, tree.cond)
    apply(env, tree.body)
  }

  private def tptApply(env: Env, tree: TptApply): Type = {
    val funTpe = apply(env, tree.fun)
    funTpe match {
      case NoType =>
        NoType
      case SimpleType(funSym, Nil) =>
        val targs = tree.targs.map {
          apply(env, _) match {
            case targ @ SimpleType(_, _) => targ
            case other => unreachable(other)
          }
        }
        SimpleType(funSym, targs)
      case other =>
        unreachable(other)
    }
  }

  private def tptId(env: Env, tree: TptId): Type = {
    env.lookup(tree.name) match {
      case NoSymbol =>
        reporter.append(UnboundId(tree))
        NoType
      case sym =>
        tree.sym = sym
        SimpleType(sym, Nil)
    }
  }

  private def tptSelect(env: Env, tree: TptSelect): Type = {
    val qualTpe = apply(env, tree.qual)
    qualTpe match {
      case NoType =>
        NoType
      case SimpleType(qualSym, Nil) =>
        val qualScope = symtab.scopes(qualSym)
        qualScope.lookup(tree.id.name) match {
          case NoSymbol =>
            reporter.append(UnboundMember(qualSym, tree.id))
            NoType
          case sym =>
            tree.id.sym = sym
            SimpleType(tree.id.sym, Nil)
        }
      case other =>
        unreachable(other)
    }
  }

  private implicit class TypecheckerTreeOps[T <: Tree](tree: T) {
    def withPos(startEnd: Tree): T = {
      tree.withPos(startEnd, startEnd)
    }

    def withPos(start: Tree, end: Tree): T = {
      tree.withPos(start.pos.input, start.pos.start, end.pos.end)
    }

    def withPos(start: Offset, end: Tree): T = {
      tree.withPos(end.pos.input, start, end.pos.end)
    }

    def withPos(start: Tree, end: Offset): T = {
      tree.withPos(start.pos.input, start.pos.start, end)
    }

    private def withPos(input: Input, start: Offset, end: Offset): T = {
      val syntheticPos = Position(input, start, end)
      if (tree.pos == NoPosition) {
        tree.pos = syntheticPos
        tree
      } else {
        unreachable(tree)
      }
    }
  }

  private implicit class TypecheckerTptOps(tpt: Tpt) {
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

  private implicit class TypecheckerSymbolOps(sym: Symbol) {
    def tpe: Type = {
      val info = symtab.infos(sym)
      info.kind match {
        case k.PACKAGE | k.OBJECT =>
          SimpleType(info.symbol, Nil)
        case k.DEF | k.VAL | k.VAR | k.PARAMETER =>
          info.tpe.get
        case _ =>
          unreachable(info)
      }
    }
  }

  private implicit class TypecheckerTpeOps(tpe: Type) {
    def subst(tparams: List[TypeParam], targs: List[Type]): Type = {
      if (tparams.isEmpty && targs.isEmpty) {
        tpe
      } else {
        tpe match {
          case NoType =>
            NoType
          case FunctionType(funTparams, funParams, funRet) =>
            val funTparams1 = funTparams.diff(tparams.map(_.id.sym))
            val funParams1 = funParams
            val funRet1 = {
              funRet.subst(tparams, targs) match {
                case funRet1 @ SimpleType(_, _) => funRet1
                case other => unreachable(other)
              }
            }
            FunctionType(funTparams1, funParams1, funRet1)
          case SimpleType(simpleSym, simpleTargs) =>
            val i = tparams.indexWhere(_.id.sym == simpleSym)
            if (i != -1) {
              targs(i)
            } else {
              val simpleSym1 = simpleSym
              val simpleTargs1 = simpleTargs.map {
                _.subst(tparams, targs) match {
                  case simpleTarg1 @ SimpleType(_, _) => simpleTarg1
                  case other => unreachable(other)
                }
              }
              SimpleType(simpleSym1, simpleTargs1)
            }
        }
      }
    }
    def subst(tparams: Seq[Symbol], targs: List[Type]): Type = {
      if (tparams.isEmpty && targs.isEmpty) {
        tpe
      } else {
        tpe match {
          case NoType =>
            NoType
          case FunctionType(funTparams, funParams, funRet) =>
            val funTparams1 = funTparams.diff(tparams)
            val funParams1 = funParams
            val funRet1 = {
              funRet.subst(tparams, targs) match {
                case funRet1 @ SimpleType(_, _) => funRet1
                case other => unreachable(other)
              }
            }
            FunctionType(funTparams1, funParams1, funRet1)
          case SimpleType(simpleSym, simpleTargs) =>
            val i = tparams.indexWhere(_ == simpleSym)
            if (i != -1) {
              targs(i)
            } else {
              val simpleSym1 = simpleSym
              val simpleTargs1 = simpleTargs.map {
                _.subst(tparams, targs) match {
                  case simpleTarg1 @ SimpleType(_, _) => simpleTarg1
                  case other => unreachable(other)
                }
              }
              SimpleType(simpleSym1, simpleTargs1)
            }
        }
      }
    }
  }

  // NOTE: This is a markedly incorrect implementation, but it allowed us
  // to typecheck re2s, so we're going to keep it for the time being.
  private def lub(tpes: List[Type]): Type = {
    tpes.distinct match {
      case List(tpe) => tpe
      case _ => SimpleType("_root_.scala.Any#", Nil)
    }
  }
}

object Typechecker {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab): Typechecker = {
    new Typechecker(settings, reporter, symtab)
  }
}
