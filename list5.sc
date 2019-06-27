//zad1
class Pair[A,B](var fst:A,var snd:B){

  override def toString = "(" + fst + ", " + snd + ")"
}

val pair1= new Pair[Int,String](1,"jeden")
pair1.toString
pair1.fst=9
pair1.snd="dziewieć"
pair1.toString=="(9, dziewieć)"
pair1.fst==9
pair1.snd=="dziewieć"

//zad2
class BankAccount(initialBalance : Double) {
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
}

//zad2a

class CheckingAccount(initialBalance:Double) extends BankAccount(initialBalance)
{
  override def deposit(amount: Double): Double = super.deposit(amount-1)

  override def withdraw(amount: Double): Double = super.withdraw(amount+1)
}

val bankAccount = new BankAccount(30)
bankAccount.checkBalance==30
bankAccount.deposit(2)
bankAccount.checkBalance==32
bankAccount.withdraw(5)
bankAccount.checkBalance==27

val checkingAccount = new CheckingAccount(30)
checkingAccount.checkBalance==30
checkingAccount.deposit(2)
checkingAccount.checkBalance==31
checkingAccount.withdraw(5)
checkingAccount.checkBalance==25

//zad2b
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance)
{
  private[this] var transactionNumber=0
  def earnMonthlyInterest =
    {
      transactionNumber=0
      super.deposit(checkBalance*0.02)
    }


  override def deposit(amount: Double): Double =
  {
    transactionNumber+=1
    super.deposit(amount - (if(transactionNumber>3) 1 else 0))
  }

  override def withdraw(amount: Double): Double =
  {
    transactionNumber+=1
      super.withdraw(amount+(if(transactionNumber>3) 1 else 0))
  }
}

val savingsAccount = new SavingsAccount(30)
savingsAccount.checkBalance ==30
savingsAccount.deposit(2)
savingsAccount.checkBalance==32
savingsAccount.withdraw(5)
savingsAccount.checkBalance==27
savingsAccount.deposit(2)
savingsAccount.checkBalance==29
savingsAccount.withdraw(5)
savingsAccount.checkBalance==23
savingsAccount.earnMonthlyInterest
savingsAccount.checkBalance==23.46
savingsAccount.withdraw(3)
savingsAccount.checkBalance==20.46

//zad3a
abstract class Zwierz(val imie:String ="bez imienia")
{
  //def rodzaj:String = this.getClass.getName
  def rodzaj:String
  def dajGlos:String
  override def toString:String=rodzaj+" "+imie +" daje glos "+dajGlos+"!"
}

//zad3b
class  Pies(imie:String="bez imienia") extends Zwierz(imie)
{
  override def rodzaj:String =  "Pies"
  override def dajGlos: String = "Hau, hau"

}


class  Kot(imie:String="bez imienia") extends Zwierz(imie)
{
  override def rodzaj:String =  "Kot"
  override def dajGlos: String = "Miau, miau"
}

class  Kruk(imie:String="bez imienia") extends Zwierz(imie)
{
  override def rodzaj:String =  "Kruk"
  override def dajGlos: String = "Kra, kra"
}


val pies = new Pies("Kruczek")
pies.toString == "Pies Kruczek daje glos Hau, hau!"
pies.imie=="Kruczek"
pies.dajGlos=="Hau, hau"
pies.rodzaj.equals("Pies")
val pies1 = new Pies()
pies1.toString=="Pies bez imienia daje glos Hau, hau!"

val kot = new Kot("Mruczek")
kot.toString=="Kot Mruczek daje glos Miau, miau!"

val kruk = new Kruk("Raven")
kruk.toString== "Kruk Raven daje glos Kra, kra!"

//zad3c
object TestZwierze {
  val pies = new Pies("Kruczek")

  val pies1 = new Pies()

  val kot = new Kot("Mruczek")

  val kruk = new Kruk("Raven")

  def main(args: Array[String]){

    val vector:Vector[Zwierz] = Vector(pies,pies1,kot,kruk)
    for(i<- vector) println( i.toString)
  }
}

TestZwierze.main(Array())