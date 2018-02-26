package week2

/**
 * Created by mateus on 6/9/15.
 */
class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = 0
  compute()

  private def compute() = {
    total = observed.map(_.currentBalance).reduce(_+_)
  }

  def handler(publisher: Publisher) = compute()

  def totalBalance = total
}
