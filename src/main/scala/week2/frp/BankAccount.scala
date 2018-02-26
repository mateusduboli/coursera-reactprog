package week2.frp

/**
 * Created by mateus on 6/9/15.
 */
class BankAccount {

  var balance = Var(0)

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val oldBalance = balance()
      balance() = oldBalance + amount
    }
  }

  def withdraw(amount: Int): Unit = {
    if (0 < amount && amount <= balance()) {
      val oldBalance = balance()
      balance() = oldBalance - amount
    } else throw new Error("insufficient funds")
  }
}
