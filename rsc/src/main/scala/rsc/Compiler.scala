// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import java.nio.file._
import rsc.lexis._
import rsc.parse._
import rsc.pretty._
import rsc.report._
import rsc.settings._
import rsc.symtab._
import rsc.syntax._
import rsc.typecheck._
import rsc.util._

class Compiler(val settings: Settings, val reporter: Reporter) extends Pretty {
  var trees: List[Source] = Nil
  var symtab: Symtab = Symtab(settings)
  var todo: Todo = Todo()

  def run(): Unit = {
    for ((taskName, taskFn) <- tasks) {
      val start = System.nanoTime()
      try {
        taskFn()
      } catch {
        case crash @ CrashException(pos, message, ex) =>
          val ex1 = if (ex != null) ex else crash
          reporter.append(CrashMessage(pos, message, ex1))
        case ex: Throwable =>
          reporter.append(CrashMessage(NoPosition, ex.getMessage, ex))
      }
      val end = System.nanoTime()
      val ms = (end - start) / 1000000
      if (settings.xprint("timings")) {
        reporter.append(VerboseMessage(s"Finished $taskName in $ms ms"))
      }
      if (settings.xprint(taskName)) {
        reporter.append(VerboseMessage(this.str))
      }
      if (settings.ystopAfter(taskName)) {
        return
      }
      if (reporter.problems.nonEmpty) {
        val numProblems = reporter.problems.length
        if (numProblems == 1) println("one error found")
        else if (numProblems == 2) println("two errors found")
        else if (numProblems == 3) println("three errors found")
        else if (numProblems == 4) println("four errors found")
        else println(s"$numProblems errors found")
        return
      }
    }
  }

  private def tasks: List[(String, () => Unit)] = List(
    "parse" -> parse,
    "schedule" -> schedule,
    "scope" -> scope,
    "outline" -> outline,
    "sign" -> sign,
    "typecheck" -> typecheck
  )

  private def parse(): Unit = {
    val inputs = settings.ins.map(in => Input(in))
    trees = inputs.flatMap { input =>
      if (Files.exists(input.path)) {
        val parser = Parser(settings, reporter, input)
        parser.accept(BOF)
        val tree = parser.source()
        parser.accept(EOF)
        Some(tree)
      } else {
        reporter.append(FileNotFound(input))
        None
      }
    }
    if (trees.isEmpty) {
      reporter.append(FilesNotFound())
    }
  }

  private def schedule(): Unit = {
    val rootEnv = Env(symtab.scopes("_root_."))

    val javaLangQual = TermSelect(TermId("java"), TermId("lang"))
    val javaLangImporter = Importer(javaLangQual, List(ImporteeWildcard()))
    val javaLangImporterScope = ImporterScope(javaLangImporter)
    todo.scopes.add(rootEnv -> javaLangImporterScope)
    val javaLangImporterEnv = javaLangImporterScope :: rootEnv

    val scalaImporter = Importer(TermId("scala"), List(ImporteeWildcard()))
    val scalaImporterScope = ImporterScope(scalaImporter)
    todo.scopes.add(javaLangImporterEnv -> scalaImporterScope)
    val scalaImporterEnv = scalaImporterScope :: javaLangImporterEnv

    val predefQual = TermSelect(TermId("scala"), TermId("Predef"))
    val predefImporter = Importer(predefQual, List(ImporteeWildcard()))
    val predefImporterScope = ImporterScope(predefImporter)
    todo.scopes.add(scalaImporterEnv -> predefImporterScope)
    val predefImporterEnv = predefImporterScope :: scalaImporterEnv

    val scheduler = Scheduler(settings, reporter, symtab, todo)
    trees.foreach(scheduler.apply(predefImporterEnv, _))
  }

  private def scope(): Unit = {
    val scoper = Scoper(settings, reporter, symtab, todo)
    while (!todo.scopes.isEmpty) {
      val (env, scope) = todo.scopes.remove()
      scope.unblock()
      if (scope.status.isPending) {
        scoper.apply(env, scope)
      }
      if (scope.status.isBlocked) {
        todo.scopes.add(env -> scope)
      }
      if (scope.status.isCyclic) {
        reporter.append(IllegalCyclicReference(scope))
      }
    }
  }

  private def outline(): Unit = {
    val outliner = Outliner(settings, reporter, symtab)
    while (!todo.mods.isEmpty) {
      val (env, mod) = todo.mods.remove()
      outliner.apply(env, mod)
    }
    while (!todo.tpts.isEmpty) {
      val (env, tpt) = todo.tpts.remove()
      outliner.apply(env, tpt)
    }
  }

  private def sign(): Unit = {
    val signer = Signer(settings, reporter)
    while (!todo.outlines.isEmpty) {
      val (env, outline) = todo.outlines.remove()
      val info = signer.apply(env, outline)
      symtab.infos(outline.id.sym) = info
    }
  }

  private def typecheck(): Unit = {
    val typechecker = Typechecker(settings, reporter, symtab)
    while (!todo.terms.isEmpty) {
      val (env, term) = todo.terms.remove()
      typechecker.apply(env, term)
    }
  }

  def printStr(p: Printer): Unit = {
    PrettyCompiler.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettyCompiler.repl(p, this)
  }
}

object Compiler {
  def apply(settings: Settings, reporter: Reporter): Compiler = {
    new Compiler(settings, reporter)
  }
}
