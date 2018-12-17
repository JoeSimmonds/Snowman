trait DrivableEvidence[T] {
  def isComplete(d : T) : Boolean
  def roll(d : T) : T
  def countOfRolls(d : T) : Int
}

object DrivableEvidence {
  implicit class DrivableOps[T : DrivableEvidence](d : T) {
    def isComplete() : Boolean = implicitly[DrivableEvidence[T]].isComplete(d)
    def roll() : T = implicitly[DrivableEvidence[T]].roll(d)
    def countOfRolls() : Int = implicitly[DrivableEvidence[T]].countOfRolls(d)

    def runToCompletion() : T = {
      if (isComplete()) d
      else roll().runToCompletion()
    }
  }
}