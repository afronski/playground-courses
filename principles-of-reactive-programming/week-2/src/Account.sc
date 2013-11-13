object Account {
  val account = new BankAccount()                 //> account  : BankAccount = BankAccount@5043cc83
  
  account deposit 50
  
  account withdraw 20                             //> res0: Int = 30
  account withdraw 20                             //> res1: Int = 10
  
  account withdraw 15                             //> java.lang.Error: Insufficient funds.
                                                  //| 	at BankAccount.withdraw(BankAccount.scala:15)
                                                  //| 	at Account$$anonfun$main$1.apply$mcV$sp(Account.scala:9)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at Account$.main(Account.scala:1)
                                                  //| 	at Account.main(Account.scala)
}