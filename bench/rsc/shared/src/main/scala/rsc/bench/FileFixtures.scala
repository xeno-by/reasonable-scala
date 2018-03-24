// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.io._
import java.nio.file._
import scala.collection.JavaConverters._

trait FileFixtures {
  lazy val buildRoot: Path = {
    BuildInfo.sourceRoot.toPath
  }

  lazy val re2sDir: Path = {
    buildRoot.resolve("examples/re2s/src/main/scala/java/util/regex")
  }

  lazy val re2sFiles: List[Path] = {
    val stream = Files.newDirectoryStream(re2sDir)
    stream.asScala.toList
  }

  lazy val stdlibDir: Path = {
    buildRoot.resolve("stdlib")
  }

  lazy val stdlibJars: List[Path] = {
    val stream = Files.newDirectoryStream(re2sDir)
    stream.asScala.toList
  }

  lazy val stdlibClasspath: String = {
    stdlibJars.map(_.toString).mkString(File.pathSeparator)
  }
}
