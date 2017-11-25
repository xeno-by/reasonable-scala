// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

final class Position(val data: Long) extends AnyVal {
  def start: Offset = (data >> 32).toInt
  def end: Offset = (data & 0xFFFFFFFFL).toInt
}

object Position {
  def apply(start: Offset, end: Offset): Position = {
    val data = start.toLong << 32 + end
    new Position(data)
  }
}

trait Positions {
  val NoPosition: Position = Position(NoOffset, NoOffset)
}
