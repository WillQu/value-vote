import org.scalajs.dom
import com.raquo.laminar.api.L.*
import valuevote.{Candidate, User, Vote}

import java.util.UUID
import scala.util.Try

object Main:
  val userList: Var[Vector[User]] = Var(initial = Vector())
  val userUpdater = userList.updater[User]((list, user) =>
    list.map(oldUser =>
      if (oldUser.id == user.id) { user }
      else { oldUser }
    )
  )
  val userDelete =
    userList.updater[User]((list, user) => list.filter(_.id != user.id))
  val updateUserCandidates =
    userList.updater[Vector[Candidate]] { (list, candidates) =>
      list.map(_.updateCandidates(candidates))
    }

  val candidates: Var[Vector[Candidate]] =
    Var(initial = Vector())
  val candidateAppender =
    candidates.updater[Candidate] { (list, candidate) =>
      list :+ candidate
    }

  val userAppender = userList.updater[User]((list, user) =>
    list :+ user.copy(votes =
      candidates.now().map(candidate => Vote(candidate, 0))
    )
  )

  val voteResult = userList.signal.map(computeResult)

  def userElement(
      userID: UUID,
      user: User,
      userSignal: Signal[User]
  ): Node =
    def voteElement(
        candidateName: String,
        vote: Vote,
        voteSignal: Signal[Vote]
    ): Node =
      val voteUpdateBus = new EventBus[Vote]
      span(
        voteUpdateBus.events
          .withCurrentValueOf(userSignal)
          .map((newVote, user) =>
            User(
              user.id,
              user.name,
              user.votes.map(userVote =>
                if (userVote.candidate == vote.candidate) { newVote }
                else { userVote }
              )
            )
          ) --> userUpdater,
        vote.candidate.name,
        input(
          controlled(
            value <-- voteSignal.map(_.value.toString),
            onInput.mapToValue.map { x =>
              Vote(vote.candidate, Try(x.toInt).getOrElse(0))
            } --> voteUpdateBus
          )
        )
      )

    div(
      input(
        controlled(
          value <-- userSignal.map(_.name),
          onInput.mapToValue.map(name => user.copy(name = name)) --> userUpdater
        )
      ),
      children <-- userSignal.map(_.votes).split(_.candidate.name)(voteElement),
      button(
        "Delete",
        onClick.mapTo {
          user
        } --> userDelete
      )
    )

  val nameInput = input()

  val candidateInput = input()

  val rootElement = div(
    div(candidates.signal --> updateUserCandidates),
    div(
      form(
        candidateInput,
        input(
          "Add candidate",
          typ := "Submit"
        ),
        onSubmit.preventDefault
          .mapTo(Candidate(candidateInput.ref.value)) --> candidateAppender
      )
    ),
    br(),
    div(
      button(
        "Add user",
        onClick.mapTo(User.create()) --> userAppender
      )
    ),
    div(
      "Users:",
      children <-- userList.signal.split(_.id)(userElement)
    ),
    child <-- voteResult.map(result => div(s"Result: $result"))
  )

  val containerNode = dom.document.querySelector("#mdoc-html-run0")

  def main(args: Array[String]): Unit =
    render(containerNode, rootElement)

  def computeResult(users: Vector[User]): String =
    val votes = users.flatMap(_.votes)
    if (votes.isEmpty) {
      "(No vote)"
    } else {
      votes
        .foldLeft(Map[String, Int]())((map, vote) =>
          map.updatedWith(vote.candidate.name)(value =>
            value.map(_ + vote.value).orElse(Some(vote.value))
          )
        )
        .maxBy(tuple => tuple._2)
        ._1
    }
