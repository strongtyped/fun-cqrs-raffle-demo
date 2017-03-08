package model.write

import io.funcqrs.config.Api._
import io.funcqrs.test.InMemoryTestSupport
import io.funcqrs.test.backend.InMemoryBackend
import model.RaffleId
import org.scalatest._

class RaffleTest extends FunSuite with Matchers with OptionValues with TryValues {

  val id = RaffleId("test-raffle")

  test("Run a Raffle") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      // send all commands
      raffle ! CreateRaffle
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! AddParticipant("George")
      raffle ! AddParticipant("Ringo")
      raffle ! Run(2)

      // assert that expected events were produced
      expectEvent[RaffleCreated]
      expectEventPF { case ParticipantAdded("John", _) => () }
      expectEventPF { case ParticipantAdded("Paul", _) => () }
      expectEvent[ParticipantAdded].name shouldBe "George"
      expectEvent[ParticipantAdded].name shouldBe "Ringo"
      expectEvent[WinnersSelected].winners should have size 2
    }

  }

  test("Run a Raffle twice") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! Run(1)

      intercept[RaffleHasAlreadyAWinner] {
        raffle ! Run(1)
      }
    }

  }

  test("Run a Raffle without participants") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle

      intercept[IllegalArgumentException] {
        raffle ! Run(1)
      }.getMessage shouldBe "Raffle has no participants"
    }

  }

  test("Add twice the same participant") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("John")

      expectEvent[RaffleCreated]
      expectEvent[ParticipantAdded]
      expectEvent[DoubleBookingRejected]
      expectNoMoreEvents()
    }

  }

  test("Reset raffle") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! RemoveAllParticipants

    }
  }

  test("Illegal to Reset a raffle that has a winner already") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! Run(1)

      intercept[RaffleHasAlreadyAWinner] {
        raffle ! RemoveAllParticipants // resetting is illegal if a winner is selected
      }
    }

  }

  test("Illegal to add new participants to a Raffle that has a winner already") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      raffle ! CreateRaffle
      raffle ! AddParticipant("John")
      raffle ! AddParticipant("Paul")
      raffle ! Run(1)

      intercept[RaffleHasAlreadyAWinner] {
        raffle ! AddParticipant("Ringo") // adding new participant is illegal if a winner is selected
      }
    }

  }

  test("Run a Raffle with 1000 participants") {

    new RaffleInMemoryTest {

      val raffle = raffleRef(id)

      // send all commands
      raffle ! CreateRaffle
      for (i <- 0 to 1000)
        raffle ! AddParticipant(s"participant $i")

      raffle ! Run(40)

      // assert that expected events were produced
      expectEvent[RaffleCreated]
      val winners = lookupExpectedEvent[WinnersSelected].winners
      winners should have size 40
      winners.toSet should have size 40
    }

  }

  class RaffleInMemoryTest extends InMemoryTestSupport {

    def configure(backend: InMemoryBackend): Unit = {
      // ---------------------------------------------
      // aggregate config - write model
      backend.configure {
        aggregate(Raffle.behavior)
      }

    }

    def raffleRef(id: RaffleId) = backend.aggregateRef[Raffle].forId(id)
  }

}
