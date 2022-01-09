package util

import scala.annotation.tailrec
import scala.collection.mutable

case class IntCodeMachine(listener: BigInt => Any = _ => {}) {

  private type Program = Map[BigInt, BigInt]
  private type CurrentIndex = BigInt
  private type RelativeBase = BigInt
  private type VariableModes = (Int, Int, Int)
  private type Variables = ((BigInt, Int), (BigInt, Int), (BigInt, Int))
  private type ProgramState = (CurrentIndex, RelativeBase, Program)

  private val inputs = mutable.Queue[BigInt]()
  private var currentState: Option[ProgramState] = None
  private var running = false

  def start(program: Seq[BigInt]): Program = execute(
    (0, 0, program.indices.map(idx => BigInt(idx) -> program(idx)).toMap.withDefaultValue(BigInt(0)))
  )

  def isRunning: Boolean = running

  def resume(input: BigInt*): Program = {
    inputs.addAll(input)
    getInstructionDetails match {
      case (op, _) =>
        if (op == 3) execute(currentState.orNull)
        else throw new IllegalStateException("Machine is not running!")
    }
  }

  private abstract class Operation {
    protected def getAddress(currentProgram: Program, value: BigInt, mode: Int, relativeBase: RelativeBase): BigInt = mode match {
      case 0 => currentProgram(value)
      case 1 => value
      case 2 => relativeBase + currentProgram(value)
    }

    def update(state: ProgramState, vars: Variables, op: (BigInt, BigInt) => BigInt): Program = (state, vars) match {
      case ((_, relativeBase, program), ((x, xMode), (y, yMode), (target, tMode))) => program + (
        getAddress(program, target, tMode, relativeBase) ->
          op(program(getAddress(program, x, xMode, relativeBase)), program(getAddress(program, y, yMode, relativeBase))))
    }

    def execute(state: ProgramState, modes: VariableModes): ProgramState
  }

  private object AddOperation extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, _), (xMode, yMode, tMode)) =>
        val nextProgram = update(state, ((currentIndex + 1, xMode), (currentIndex + 2, yMode), (currentIndex + 3, tMode)), _ + _)
        (currentIndex + 4, relativeBase, nextProgram)
    }
  }

  private object MultiplyOperation extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, _), (xMode, yMode, tMode)) =>
        val nextProgram = update(state, ((currentIndex + 1, xMode), (currentIndex + 2, yMode), (currentIndex + 3, tMode)), _ * _)
        (currentIndex + 4, relativeBase, nextProgram)
    }
  }

  private object InputOperation extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, program), (xMode, _, _)) =>
        val nextProgram = program + (getAddress(program, currentIndex + 1, xMode, relativeBase) -> inputs.dequeue())
        (currentIndex + 2, relativeBase, nextProgram)
    }
  }

  private object OutputOperation extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, program), (xMode, _, _)) =>
        listener(program(getAddress(program, currentIndex + 1, xMode, relativeBase)))
        (currentIndex + 2, relativeBase, program)
    }
  }

  private object AdjustBaseOperation extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, program), (xMode, _, _)) =>
        (currentIndex + 2, relativeBase + program(getAddress(program, currentIndex + 1, xMode, relativeBase)), program)
    }
  }

  private abstract class Jump extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, program), (xMode, yMode, _)) =>
        if (shouldJump(program, getAddress(program, currentIndex + 1, xMode, relativeBase)))
          (program(getAddress(program, currentIndex + 2, yMode, relativeBase)), relativeBase, program)
        else (currentIndex + 3, relativeBase, program)
    }

    def shouldJump(program: Program, xAdd: BigInt): Boolean
  }

  private object JumpIfTrue extends Jump {
    def shouldJump(program: Program, xAdd: BigInt): Boolean = program(xAdd) > 0
  }

  private object JumpIfFalse extends Jump {
    def shouldJump(program: Program, xAdd: BigInt): Boolean = program(xAdd) == 0
  }

  private abstract class ConditionalStore extends Operation {
    def execute(state: ProgramState, modes: VariableModes): ProgramState = (state, modes) match {
      case ((currentIndex, relativeBase, program), (xMode, yMode, tMode)) =>
        val valueToUpdate =
          if (shouldStore(program, getAddress(program, currentIndex + 1, xMode, relativeBase),
            getAddress(program, currentIndex + 2, yMode, relativeBase))) 1 else 0

        (currentIndex + 4, relativeBase, program + (getAddress(program, currentIndex + 3, tMode, relativeBase) -> valueToUpdate))
    }

    def shouldStore(program: Program, xAdd: BigInt, yAdd: BigInt): Boolean
  }

  private object StoreIfLessThan extends ConditionalStore {
    override def shouldStore(program: Program, xAdd: BigInt, yAdd: BigInt): Boolean = program(xAdd) < program(yAdd)
  }

  private object StoreIfEqual extends ConditionalStore {
    override def shouldStore(program: Program, xAdd: BigInt, yAdd: BigInt): Boolean = program(xAdd) == program(yAdd)
  }

  private val ops: Map[Int, Operation] = Map(
    1 -> AddOperation,
    2 -> MultiplyOperation,
    3 -> InputOperation,
    4 -> OutputOperation,
    5 -> JumpIfTrue,
    6 -> JumpIfFalse,
    7 -> StoreIfLessThan,
    8 -> StoreIfEqual,
    9 -> AdjustBaseOperation
  )

  private def getInstructionDetails: (Int, VariableModes) = this.currentState match {
    case Some((currentIndex, _, currentProgram)) =>
      val instruction = currentProgram(currentIndex)
      ((instruction % 100).toInt, (((instruction / 100) % 10).toInt, ((instruction / 1000) % 10).toInt, ((instruction / 10000) % 10).toInt))
  }

  @tailrec
  private def execute(state: ProgramState): Program = {
    this.currentState = Option(state)
    getInstructionDetails match {
      case (operation, variableModes) =>
        if ((operation == 3 && inputs.isEmpty) || operation == 99) {
          this.running = operation != 99
          currentState match {
            case Some((_, _, program)) => program
          }
        } else {
          ops(operation).execute(state, variableModes) match {
            case nextState => execute(nextState)
          }
        }
    }
  }

}