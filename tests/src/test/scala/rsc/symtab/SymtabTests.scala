// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import scala.compat.Platform.EOL
import utest._
import rsc.semantics.SymbolInformation.Kind._
import rsc.semantics.SymbolInformation.Property._
import rsc.settings._
import rsc.tests._

object SymtabTests extends RscTests {
  var symtab: Symtab = null
  val tests = Tests {
    "initialize" - {
      val settings = Settings.parse(List("-cp", stdlibClasspath)).get
      symtab = Symtab(settings)
    }
    "infos(scala.Int#)" - {
      val int = symtab.infos("scala.Int#")
      assert(int.kind == CLASS)
      assert(int.properties == (ABSTRACT.value | FINAL.value))
    }
    "members(scala.sys.Prop#)" - {
      val scope = symtab.scopes("scala.sys.Prop#")
      val obtained = scope.members.mkString(EOL)
      assert(obtained == """
        |scala.sys.Prop#key().
        |scala.sys.Prop#value().
        |scala.sys.Prop#[T]
        |scala.sys.Prop#isSet().
        |scala.sys.Prop#set(String).
        |scala.sys.Prop#setValue(T1).
        |scala.sys.Prop#get().
        |scala.sys.Prop#option().
        |scala.sys.Prop#clear().
        |scala.sys.Prop#zero().
        |scala.AnyRef#`<init>`().
        |scala.AnyRef#eq(AnyRef).
        |scala.AnyRef#ne(AnyRef).
        |scala.AnyRef#synchronized(T).
        |scala.Any#`<init>`().
        |scala.Any#equals(Any).
        |scala.Any#`==`(Any).
        |scala.Any#`!=`(Any).
        |scala.Any#hashCode().
        |scala.Any#`##`().
        |scala.Any#toString().
        |scala.Any#getClass().
        |scala.Any#isInstanceOf().
        |scala.Any#asInstanceOf().
      """.trim.stripMargin)
    }
    "infos(java.util.Queue#)" - {
      val queue = symtab.infos("java.util.Queue#")
      assert(queue.kind == INTERFACE)
      assert(queue.properties == ABSTRACT.value)
    }
    "scopes(java.util.Queue#)" - {
      assert(symtab.scopes("java.util.Queue#") != null)
    }
    "members(scala.util.control.NoStackTrace#)" - {
      val scope = symtab.scopes("scala.util.control.NoStackTrace#")
      val obtained = scope.members.mkString(EOL)
      assert(obtained.nonEmpty)
    }
  }
}
