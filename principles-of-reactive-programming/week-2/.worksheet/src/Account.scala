object Account {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(50); 
  val account = new BankAccount();System.out.println("""account  : BankAccount = """ + $show(account ));$skip(24); 
  
  account deposit 50;$skip(25); val res$0 = 
  
  account withdraw 20;System.out.println("""res0: Int = """ + $show(res$0));$skip(22); val res$1 = 
  account withdraw 20;System.out.println("""res1: Int = """ + $show(res$1));$skip(25); val res$2 = 
  
  account withdraw 15;System.out.println("""res2: Int = """ + $show(res$2))}
}
