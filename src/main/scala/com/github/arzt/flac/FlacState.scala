package com.github.arzt.flac

trait FlacState:
  def apply(byte: Byte): FlacState

case class FlacErrorState(state: FlacState, byte: Byte, message: String) extends FlacState:
  override def apply(byte: Byte): FlacState = this

case class FlacStart() extends FlacState:
  def apply(byte: Byte): FlacState =
    if byte == 'f' then FlacFState() else FlacErrorState(this, byte, "")

case class FlacFState() extends FlacState:
  override def apply(byte: Byte): FlacState =
    if byte == 'L' then FlacLState() else FlacErrorState(this, byte, "")

case class FlacLState() extends FlacState:
  override def apply(byte: Byte): FlacState =
    if byte == 'a' then FlacAState() else FlacErrorState(this, byte, "")

case class FlacAState() extends FlacState:
  override def apply(byte: Byte): FlacState =
    if byte == 'C' then FlacLState() else FlacErrorState(this, byte, "")


object FlacState:

  def apply(): FlacState = FlacStart()
