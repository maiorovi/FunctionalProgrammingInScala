package chapter4

class PersonExample extends App {

  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("name is empty.")
    else Right(new Name(name))

  def mkAge(age:Int):Either[String, Age] =
    if(age == null || age < 0) Left("age is null or out of range")
    else Right(new Age(age))

  def mkPerson(name:String, age:Int):Either[String, Person] =
    mkName(name).map2(mkAge(age))(new Person(_, _))
}
