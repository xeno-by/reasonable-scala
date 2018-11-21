// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v1

import scalafix.v1._

abstract class SemanticdbRule(name: String)
    extends SemanticRule(name) {

  def index(implicit semanticDoc: SemanticDocument): DocumentIndex =
    new DocumentIndex(semanticDoc.internal)

}
