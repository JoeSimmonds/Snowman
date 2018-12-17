case class Snowman(body: Boolean, head: Boolean, arms: Int, buttons: Int, eyes: Int, nose: Boolean, hat: Boolean, history : Seq[SnowmanRoll]) {
  def apply(roll : SnowmanRoll) : Snowman =  {
    ((this, roll) match {
      case (Snowman(false, _, _, _, _, _, _, _), SnowmanRoll.Body)                 => copy(body = true)
      case (Snowman(true, false, _, _, _, _, _, _), SnowmanRoll.Head)              => copy(head = true)
      case (Snowman(_, true, _, _, _, _, false, _), SnowmanRoll.Hat)              => copy(hat = true)
      case (Snowman(true, _, _, b, _, _, _, _), SnowmanRoll.ButtonOrEye) if b < 3  => copy(buttons = b+1)
      case (Snowman(_, true, _, _, e, _, _, _), SnowmanRoll.ButtonOrEye) if e < 2  => copy(eyes = e+1)
      case (Snowman(true, _, a, _, _, _, _, _), SnowmanRoll.Arm) if a < 2          => copy(arms = a+1)
      case (Snowman(_, true, _, _, _, false, _, _), SnowmanRoll.CarrotNose)        => copy(nose = true)
      case _ => this
    }).copy(history = history :+ roll)
  }

  def isComplete() : Boolean = {
    this match {
      case Snowman(true, true, 2, 3, 2, true, true, _) => true
      case _ => false
    }
  }

  def equalsExceptHistory(that : Snowman): Boolean =
    this.body == that.body &&
    this.head == that.head &&
    this.arms == that.arms &&
    this.buttons == that.buttons &&
    this.eyes == that.eyes &&
    this.nose == that.nose &&
    this.hat == that.hat

  def isPerfectRun : Boolean = isComplete() && countOfRolls==11

  def countOfRolls : Int = history.size
}

object Snowman {
  def empty():Snowman  = {
    Snowman(false, false, 0, 0, 0, false, false, Seq.empty)
  }

  implicit val snowmanIsDrivable : DrivableEvidence[Snowman] =
    new DrivableEvidence[Snowman] {
      override def isComplete(d: Snowman): Boolean = d.isComplete()
      override def roll(d: Snowman): Snowman = d(SnowmanRoll.random())
      override def countOfRolls(d: Snowman): Int = d.countOfRolls
    }
}