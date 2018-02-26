package week2

/**
 * Created by mateus on 6/9/15.
 */
class BankAccount extends Publisher{

  private var balance = 0

  def currentBalance = balance

  def withdraw(amount: Int): Unit = {
    if(0 < amount && amount <= balance) balance -= amount
    else throw new Error("insufficient funds")
    publish()
  }

  def deposit(amount: Int): Unit = {
    if (amount > 0) balance += amount
    publish()
  }
}
