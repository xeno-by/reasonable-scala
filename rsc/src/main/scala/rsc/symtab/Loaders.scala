// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.net._
import java.nio.file._
import java.util.jar._
import java.util.HashMap
import rsc.semantics._
import rsc.semantics.SymbolInformation.{Kind => k}
import rsc.typecheck._
import rsc.util._

trait Loaders {
  self: Symtab =>

  private val todo = new HashMap[Symbol, URL]
  private val language = Some(Language(name = "Reasonable Scala"))

  protected def scanClasspath(classpath: List[Path]): Unit = {
    val packages = List.newBuilder[Scope]
    classpath.foreach { entryPath =>
      def consumeIndex(prefix: String, index: Index): Unit = {
        index.packages.foreach { p =>
          var scope = _scopes._storage.get(p.symbol)
          if (scope == null) {
            scope = PackageScope(p.symbol)
            packages += scope
            _scopes._storage.put(p.symbol, scope)

            val i = p.symbol.lastIndexOf('.', p.symbol.length - 2)
            val name = p.symbol.substring(i + 1, p.symbol.length - 1)
            val owner = p.symbol.substring(0, i + 1)
            val info = SymbolInformation(
              symbol = p.symbol,
              language = language,
              kind = k.PACKAGE,
              name = name,
              owner = owner
            )
            _infos._storage.put(p.symbol, info)
          }
          p.members.foreach { m =>
            val name = Name(m.substring(p.symbol.length))
            val m1 = scope.enter(name, m)
            if (m1 != NoSymbol && m1 != m) unreachable(m)
          }
        }
        index.toplevels.foreach { t =>
          todo.put(t.symbol, new URL(prefix + t.uri))
        }
      }
      if (Files.isDirectory(entryPath)) {
        val indexPath = entryPath.resolve("META-INF/semanticdb.semanticidx")
        if (Files.exists(indexPath)) {
          val index = {
            val indexStream = Files.newInputStream(indexPath)
            try Index.parseFrom(indexStream)
            finally indexStream.close()
          }
          val prefix = entryPath.toUri.toString + "/META-INF/semanticdb/"
          consumeIndex(prefix, index)
        }
      } else if (entryPath.toString.endsWith(".jar")) {
        val jar = new JarFile(entryPath.toFile)
        val indexEntry = jar.getEntry("META-INF/semanticdb.semanticidx")
        if (indexEntry != null) {
          val index = {
            val indexStream = jar.getInputStream(indexEntry)
            try Index.parseFrom(indexStream)
            finally indexStream.close()
          }
          val prefix = "jar:" + entryPath.toUri.toString + "!/META-INF/semanticdb/"
          consumeIndex(prefix, index)
        }
      } else {
        ()
      }
    }
    packages.result.foreach(_.succeed())
  }

  protected def loadFromClasspath(sym: Symbol): Unit = {
    val url = todo.get(sym)
    if (url != null) {
      val semanticdb = {
        val semanticdbStream = url.openStream()
        try TextDocuments.parseFrom(semanticdbStream)
        finally semanticdbStream.close()
      }
      semanticdb.documents.foreach { document =>
        document.symbols.foreach { info =>
          info.kind match {
            case k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT =>
              val scope = SemanticdbScope(info, this)
              _scopes._storage.put(info.symbol, scope)
            case _ =>
              ()
          }
          _infos._storage.put(info.symbol, info)
        }
      }
    }
  }
}
