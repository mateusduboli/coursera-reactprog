import week2.{Consolidator, BankAccount}
object bankaccount {
  val a = new BankAccount
  val b = new BankAccount
  val c = new Consolidator(List(a,b))

  a deposit 20

  c.totalBalance

  b deposit 30

  c.totalBalance
}