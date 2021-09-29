package valuevote

import java.util.UUID

final case class User(id: UUID, name: String, votes: Vector[Vote]):
  def candidates: Vector[Candidate] = votes.map(_.candidate)

  def addCandidate(candidate: Candidate): User =
    if (!this.candidates.contains(candidate)) {
      User(id, name, votes :+ Vote(candidate, 0))
    } else {
      this
    }

  def updateCandidates(candidates: Vector[Candidate]): User =
    candidates
      .foldLeft(
        this.copy(votes =
          this.votes.filter(vote => candidates contains vote.candidate)
        )
      )(_ addCandidate _)

object User:
  def create(): User = User(UUID.randomUUID(), "", Vector())
