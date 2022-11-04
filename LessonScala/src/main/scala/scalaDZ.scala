package step34
import scala.io.StdIn.readLine
import scalaj.http._

object scalaDZ {
  def main(args: Array[String]): Unit = {
    val hello  = "Hello, Scala!"
    println("============= // a1 // ===============")
    println(hello.reverse)
    println("============= // a2 // ===============")
    println(hello.toLowerCase())
    println("============= // a3 // ===============")
    println(hello.init)
    println("============= // a4 // ===============")
    println(hello.init + " " + "and goodbye python!")
    println("============= // b // ===============")
    println("Введите годовой доход, размер премии (%) и % дотации на питание через пробел:")
    var data: List[String] = readLine().split("\\s+").toList
    val salaryOnYear= data(0).toDouble
    val bonusOnYear = data(1).toDouble/100
    val bonusEat = data(2).toDouble

    def salary(x: Double, y: Double, be: Double):Double = {
    var salaryGross: Double = x + x*y + be
    val salaryOnMonth: Double= salaryGross * 0.87 / 12
      salaryOnMonth
    }
    val salaryOnMonth = salary(x=salaryOnYear, y = bonusOnYear, be = bonusEat)
    println(s"Месячный доход сотрудника - $salaryOnMonth руб.")
    println("============= // c // ===============")
    val peopleListSalary: List[Double] = List(100.0, 150.0, 200.0, 80.0, 120.0, 75.0)
    val meanSalary = peopleListSalary.sum / peopleListSalary.length
    for (i <- peopleListSalary.indices) {
      var deviationSalary: Double = (peopleListSalary(i) - meanSalary)
      var div: Double = deviationSalary * 100 / meanSalary
      if (div < 0) {
        div = div*(-1)
        println(s"Отклонение на $div % меньше среднего")
      } else if (div > 0) {
        println(s"Отклонение на $div % больше среднего")
      } else {
        println(s"Средний оклад")
      }
    }
    println("============= // d // ===============")
    println("Индекс сотрудника, которого рассмотрим?")
    val user_id = readLine().toInt
    println("Как вел себя сотрудник?")
    val b = readLine().toLowerCase()

    def bonusOrnot(uid: Int, b: String, salUser: List[Double]): List[Double] = {
      val bb: Double = 2.5;
      if (b == "good") {
        val temp: Double = salUser(uid) + salUser(uid) * bb
        salUser.updated(uid, temp)
      }
      else if (b == "bad") {
        val temp: Double = salUser(uid) + salUser(uid) * bb
        salUser.updated(uid, temp)
      }
      else {
        println("Не поощрим!")
        salUser
      }
    }
    val peopleListSalaryNew = bonusOrnot(uid = user_id, b = b, salUser = peopleListSalary)
    println(peopleListSalaryNew.max, peopleListSalaryNew.min)
    println("============= // e // ===============")
    val peopleListSalaryNewSort = (peopleListSalaryNew ::: List(90.0, 350.0)).sorted
    println(peopleListSalaryNewSort)
    println("============= // f // ===============")
    println("Введите индекс элемента для вставки:")
    val i: Int = readLine().toInt
    def insert[T](list: List[T], i: Int, value: T) = {
      list.take(i) ++ List(value) ++ list.drop(i)
    }
    val peopleListSalaryNewSortOneUserNew: List[Double] = insert(peopleListSalaryNewSort, i = i, value = 130.0)
    println(peopleListSalaryNewSortOneUserNew)
    println("============= // g // ===============")
    println("June: 100 тыс - 150 тыс\nMiddle: 160 тыс - 200 тыс\nSenior: > 200 тыс")
    for (i <-peopleListSalaryNewSortOneUserNew.indices) {
      if ((peopleListSalaryNewSortOneUserNew(i) >= 150.0) & (peopleListSalaryNewSortOneUserNew(i) <= 200.0)) {
        (println(i))
      }
    }
    println("============= // h // ===============")
    val inexed_value: Double = 0.7
    val salaryIndexed: List[Double] = peopleListSalaryNewSortOneUserNew.map(x => x * inexed_value + x)
    println(salaryIndexed)
  }
}

