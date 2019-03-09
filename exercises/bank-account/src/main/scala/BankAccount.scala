trait BankAccount {

  def closeAccount(): Unit

  def getBalance: Option[Int]

  def incrementBalance(increment: Int): Option[Int]
}

object Bank {
  def openAccount(): BankAccount = new BankAccount {
    private var balance: Option[Int] = Some(0)

    override def closeAccount() = synchronized {
      balance = None
    }

    override def getBalance = synchronized { balance }

    override def incrementBalance(increment: Int) = synchronized {
      balance = balance.map(_ + increment)
      balance
    }
  }
}

