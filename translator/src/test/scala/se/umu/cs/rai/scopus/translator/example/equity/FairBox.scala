package se.umu.cs.rai.scopus.translator.example.equity


trait Box {
  val height: Double
}


trait Participant {
  val height: Double
}


case class Box_assignment (participant: Participant, box: Box)

/*
  It is assumed that the boxes suffice for every one.
*/

case class FairBox (
  distribute: (Seq[Box], Seq[Participant]) => Seq[Box_assignment],
  fence_height: Double,
) {


  def is_fair (resources: Seq[Box], participants: Seq[Participant]) = {
    def assignments = distribute (resources, participants)
    def satisfied_participants = count_satisfied (participants, assignments)
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
      .reduce ((height1, height2) => height1 + height2)


  def count_satisfied (participants: Seq[Participant], assignments: Seq[Box_assignment]) =
    participants
      .filter (participant => participant.height + height_enhancement (assignments, participant) > fence_height)
      .length

}
