// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.io._
import java.nio.file._
import scala.collection.JavaConverters._
import sys.process._

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

  lazy val stdlibClasspath: String = {
    val repo = "https://oss.sonatype.org/content/repositories/staging"
    def fetch(artifact: String): File = {
      val command = s"coursier fetch $artifact -r $repo"
      // println(s"Fetching $artifact...")
      val stdout = command.!!.trim
      new File(stdout.split("\n").last)
    }
    def detectJdk(): List[File] = {
      // println("Detecting JDK...")
      val bootcp = sys.props.collectFirst {
        case (k, v) if k.endsWith(".boot.class.path") =>
          v.split(java.io.File.pathSeparatorChar).toList.map(e => new File(e))
      }
      bootcp.getOrElse(sys.error("failed to detect JDK"))
    }
    def downloadScalalib(): List[File] = {
      List(fetch("org.scala-lang:scala-library:2.12.4"))
    }
    def metacp(entries: List[File]): String = {
      val classpath = entries.mkString(java.io.File.pathSeparator)
      val metacp = s"org.scalameta:metacp_2.12:3.5.0-61-3fa7f544-SNAPSHOT"
      val command = s"coursier launch $metacp -r $repo -- --include-scala-library-synthetics $classpath"
      // println(s"Converting $classpath...")
      command.!!.trim
    }
    val classpath = detectJdk() ++ downloadScalalib()
    metacp(classpath)
  }
}
