package com.rockthejvm.part1recap

object ContextualAbstractionsScala2 {

  // implicit classes
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String =
      Person(name).greet()
  }

  // extension method
  val greeting = "Peter".greet()  // method available on the string "Peter" => new ImpersonableString("Peter").greet() -- extension method equivalent

  // example scala.concurrent.duration
  import scala.concurrent.duration._
  val oneSecond = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount: Int = 10

  val twelve = increment(2)  // implicit argument 10 passed in by the compiler

  def multiply(x: Int)(implicit factor: Int): Int = x * factor
  val aHundred = multiply(10)

  // more complex example
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  def convert2JSON[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJSON(value)

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJSON(p: Person) = "{\"name\" : \"" + p.name + "\"}"
  }

  val davidsJson = convert2JSON(Person("David")) // implicit serializer passed here

  // implicit defs
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJSON(xs: List[T]) = s"[${xs.map(serializer.toJSON).mkString(",")}]"
    }

  val personsJSON = convert2JSON(List(Person("alice"), Person("Bob")))

  // implicit conversions (not recommended)
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  implicit def string2Cat(name: String): Cat = Cat(name)

  val aCat: Cat = "Garfield" // compiler sees that a string => cat method exists, so it will allow this (string2Cat("Garfield"))
  val garfieldMeowing = "Garfield".meow() // allows the pass through conversion, which is not a good idea

  def main(args: Array[String]): Unit = {
    println(davidsJson)
    println(personsJSON)
  }
}

object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  // part 1: Type class definition
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  // part 2: Type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJSON(value: String) = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJSON(value: Int) = "\"" + value.toString + "\""
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJSON(value: Person) =
      s"""
         |{ "name": "${value.name}", "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // part 3: Offer some API
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJSON(value)

  def convertListToJson[T](xs: List[T])(implicit serializer: JSONSerializer[T]): String =
    xs.map(value => serializer.toJSON(value)).mkString("[", ",", "]")

  // part 4: Add extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJSON(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJson(List(Person("Alice", 23), Person("Bob", 42))))
    val xavier = Person("Xavier", 68)
    import JSONSyntax._
    println(xavier.toJson)
  }
}