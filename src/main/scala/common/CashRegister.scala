package common

object CashRegister extends App {

  def checkCashRegister(bill: Double, payment: Double, CurrentCashRegister: Seq[(String, Double)]): Seq[(String, Double)] = {
    val changeToReturn = payment - bill
    val register = CurrentCashRegister.map(e => e._1 -> e._2).toMap

    def calcChange(changeToReturn: Double, cashRegister: Map[String, Double], changeBack:  Seq[(String, Double)]): Seq[(String, Double)] = {

      def getLeftOver(cashType: String, singleValue: Double): (Double, Double) = {
        val newChangeToReturn = changeToReturn - singleValue
        val leftOverCash = cashRegister(cashType) - singleValue
        (newChangeToReturn, leftOverCash)
      }

      if(changeToReturn >= 100.0 && cashRegister("ONE HUNDRED") != 0) {
        val r = getLeftOver("ONE", 100.0)
        calcChange(r._1, cashRegister ++ Map("ONE HUNDRED" -> r._2), changeBack ++ Seq(("ONE HUNDRED", 100.0)))
      }
      else if(changeToReturn >= 20.0 && cashRegister("TWENTY") != 0) {
        val r = getLeftOver("TWENTY", 20.0)
        calcChange(r._1, cashRegister ++ Map("TWENTY" -> r._2), changeBack ++ Seq(("TWENTY", 20.0)))
      }
      else if(changeToReturn >= 10.0 && cashRegister("TEN") != 0) {
        val r = getLeftOver("TEN", 1.0)
        calcChange(r._1, cashRegister ++ Map("TEN" -> r._2), changeBack ++ Seq(("TEN", 10.0)))
      }
      else if(changeToReturn >= 5.0 && cashRegister("FIVE") != 0) {
        val r = getLeftOver("FIVE", 5.0)
        calcChange(r._1, cashRegister ++ Map("FIVE" -> r._2), changeBack ++ Seq(("FIVE", 5.0)))
      }
      else if(changeToReturn >= 1.0 && cashRegister("ONE") != 0) {
        val r = getLeftOver("ONE", 1.0)
        calcChange(r._1, cashRegister ++ Map("ONE" -> r._2), changeBack ++ Seq(("ONE", 1.0)))
      }
      else if(changeToReturn >= 0.25 && cashRegister("QUARTER") != 0) {
        val r = getLeftOver("QUARTER", 0.25)
        calcChange(r._1, cashRegister ++ Map("QUARTER" -> r._2), changeBack ++ Seq(("QUARTER", 0.25)))
      }
      else if(changeToReturn >= 0.10 && cashRegister("DIME") != 0) {
        val r = getLeftOver("DIME", 0.1)
        calcChange(r._1, cashRegister ++ Map("DIME" -> r._2), changeBack ++ Seq(("DIME", 0.1)))
      }
      else if(changeToReturn >= 0.05 && cashRegister("NICKEL") != 0) {
        val r = getLeftOver("NICKEL", 0.05)
        calcChange(r._1, cashRegister ++ Map("NICKEL" -> r._2), changeBack ++ Seq(("NICKEL", 0.05)))
      }
      else if(changeToReturn >= 0.01 && cashRegister("PENNY") != 0) {
        val r = getLeftOver("PENNY", 0.01)
        calcChange(r._1, cashRegister ++ Map("PENNY" -> r._2), changeBack ++ Seq(("PENNY", 0.01)))
      } else {
        changeBack
      }

    }

    calcChange(changeToReturn, register, Seq.empty[(String, Double)])
      .groupBy(e => e._1)
      .mapValues(e => e.map(_._2).sum).toSeq
  }


  val currentCash: Seq[(String, Double)] = Seq(("PENNY", 1.01), ("NICKEL", 2.05), ("DIME", 3.1), ("QUARTER", 4.25), ("ONE", 90), ("FIVE", 55), ("TEN", 20), ("TWENTY", 60), ("ONE HUNDRED", 100))
  println(checkCashRegister(19.77, 20, currentCash));
  println(checkCashRegister(15.50, 20, currentCash));
  println(checkCashRegister(100.77, 150, currentCash));
  println(checkCashRegister(320.50, 350, currentCash));

}
