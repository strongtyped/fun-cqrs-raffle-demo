package model.write

import io.funcqrs.config.Api._
import io.funcqrs.test.InMemoryTestSupport
import io.funcqrs.test.backend.InMemoryBackend
import model.RaffleId
import org.scalatest._

class RaffleTest extends FunSuite with Matchers with OptionValues with TryValues {

  import RaffleProtocol._

  val id = RaffleId("test-raffle")

  test("Run a Raffle") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      // send all commands
      raffle ! CreateRaffle(2)
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! AddParticipant("George")
      raffle ! AddParticipant("Ringo")
      raffle ! Run

      // assert that expected events were produced
      expectEvent[RaffleCreated]
      expectEventPF { case ParticipantAdded("John", _) => () }
      expectEventPF { case ParticipantAdded("Paul", _) => () }
      expectEvent[ParticipantAdded].name shouldBe "George"
      expectEvent[ParticipantAdded].name shouldBe "Ringo"
      expectEvent[WinnerSelected].winners should have size 2
    }

  }

  test("Run a Raffle twice") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle(1)
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! Run

      intercept[RaffleHasAlreadyAWinner] {
        raffle ! Run
      }
    }

  }

  test("Run a Raffle without participants") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle(1)

      intercept[IllegalArgumentException] {
        raffle ! Run
      }.getMessage shouldBe "Raffle has no participants"
    }

  }

  test("Add twice the same participant") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle(1)
      raffle ! AddParticipant("John")

      intercept[IllegalArgumentException] {
        raffle ! AddParticipant("John")
      }.getMessage shouldBe "Participant John already added!"
    }

  }

  test("Reset raffle") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle(1)
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")

      raffle ! RemoveAllParticipants

    }

  }

  test("Illegal to Reset a raffle that has a winner already") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle(1)
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! Run

      intercept[RaffleHasAlreadyAWinner] {
        raffle ! RemoveAllParticipants // resetting is illegal if a winner is selected
      }
    }

  }

  test("Illegal to add new participants to a Raffle that has a winner already") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle(1)
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! Run

      intercept[RaffleHasAlreadyAWinner] {
        raffle ! AddParticipant("Ringo") // adding new participant is illegal if a winner is selected
      }
    }

  }

  class RaffleInMemoryTest extends InMemoryTestSupport {

    def configure(backend: InMemoryBackend): Unit = {
      // ---------------------------------------------
      // aggregate config - write model
      backend.configure {
        aggregate[Raffle](Raffle.behavior)
      }

    }

    def raffleRef(id: RaffleId) = aggregateRef[Raffle](id)
  }

}
