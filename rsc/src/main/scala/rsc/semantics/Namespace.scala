// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._

trait Namespace extends Pretty {
  def lookup(name: Name): Symbol
}
