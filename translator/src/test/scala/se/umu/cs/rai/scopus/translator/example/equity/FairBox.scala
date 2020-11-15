package se.umu.cs.rai.scopus.translator.example.equity

/**
  * This class models a box.
  */
trait Box {
  val height: Double
}

/**
  * This class models a participant.
  */
trait Participant {
  val height: Double
}


case class Box_assignment (participant: Participant, box: Box)

/**
  * This models the problem of assigning boxes in a fair way.
  * It is assumed that the boxes suffice for every one.
  */
case class FairBox (
  distribute: (Seq[Box], Seq[Participant]) => Seq[Box_assignment],
  fence_height: Double,
) {

  /** Tells if the distribution is fair or not. */
  def is_fair (resources: Seq[Box], participants: Seq[Participant]) = {
    val assignments = distribute (resources, participants)
    val satisfied_participants = count_satisfied (participants, assignments)
    val number_of_participants = participants.length
    if ( satisfied_participants == number_of_participants
    ) true
    else false
  }


  def assignments_for_participant (assignments: Seq[Box_assignment], participant: Participant) =
    assignments
      .filter (assignment => participant == assignment.participant)


  def height_enhancement (assignments: Seq[Box_assignment], participant: Participant) =
    assignments_for_participant (assignments, participant)
      .map (assignment => assignment.box.height)
      .sum


  def count_satisfied (participants: Seq[Participant], assignments: Seq[Box_assignment]) =
    participants
      .filter (participant => participant.height + height_enhancement (assignments, participant) > fence_height)
      .length

}
