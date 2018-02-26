import week2.frp.{Signal, BankAccount}

def consolidated(accounts: List[BankAccount]): Signal[Int] = {
  Signal(accounts.map(_.balance()).sum)
}

val a = new BankAccount
val b = new BankAccount
val c = consolidated(List(a,b))
c()
a deposit 20
c()
