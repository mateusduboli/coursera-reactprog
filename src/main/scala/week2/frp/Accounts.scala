package week2.frp

/**
 * Created by mateus on 6/12/15.
 */
object Accounts extends App {

  def consolidated(accounts: List[BankAccount]): Signal[Int] = {
    Signal {
      accounts.map { account =>
        account.balance()
      }.sum
    }
  }

  override def main(args: Array[String]) {
    val a = new BankAccount
    val b = new BankAccount
    val c = consolidated(List(a,b))
    println(s"Total accounts value: ${c()}")
    a deposit 20
    println(s"Total accounts value: ${c()}")
  }
}
