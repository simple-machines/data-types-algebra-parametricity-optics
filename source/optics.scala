package p
object Infra {

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B): F[A] => F[B]
  }

  trait Applicative[F[_]] {
    def pure[A]: A => F[A]
    def ap[A, B](f: F[A => B]): F[A] => F[B]
  }
}

object Optics {
import Infra._
case class Person(name: String, age: Int)

case class VehicleRegistration(num: String, owner: Person)

case class Vehicle(make: String, reg: VehicleRegistration)

// Use-case: the vehicle's owner has just had a birthday

def birthday_naive: Vehicle => Vehicle = {
  case Vehicle(make, reg) =>
    reg match {
      case VehicleRegistration(num, owner) =>
        owner match {
          case Person(name, age) =>
            Vehicle(make, VehicleRegistration(num, Person(name, age+1)))
        }
    }
  }

def modifyVehicleRegistration1: 
  (VehicleRegistration => VehicleRegistration) =>
  Vehicle => Vehicle =
  k => {
    case Vehicle(make, reg) =>
      Vehicle(make, k(reg))
  }

def modifyOwner1:
  (Person => Person)
  => VehicleRegistration
  => VehicleRegistration =
  k => {
    case VehicleRegistration(num, owner) =>
      VehicleRegistration(num, k(owner))
  }

def modifyAge1: 
  (Int => Int)
  => Person
  => Person =
  k => {
    case Person(name, age) =>
      Person(name, k(age))
  }

def modifyVehicleOwnerAge1:
  (Int => Int)
  => Vehicle
  => Vehicle =
  modifyVehicleRegistration1 compose modifyOwner1 compose modifyAge1

def birthday_composed: Vehicle => Vehicle =
  modifyVehicleOwnerAge1(_+1)

// Vehicle has exactly one VehicleRegistration and some other things
// VehicleRegistration has exactly one Person and some other things
// Person has exactly one Int and some other things
//        has exactly one     **and** some other things

sealed trait ParseError
case class UnexpectedEOF() extends ParseError
case class UnexpectedChar(expected: Char, actual: Char) extends ParseError

sealed trait ParseResult[A]
case class SuccessResult[A](a: A) extends ParseResult[A]
case class ErrorResult[A](e: ParseError) extends ParseResult[A]

// Use-case: upper-case a ParseResult's actual character

def modifyActualChar: (Char => Char) => (Char, Char) => (Char, Char) =
  k => {
    case (e, a) => (e, k(a))
  }

def modifyChars: ((Char, Char) => (Char, Char)) => ParseError => ParseError =
  k => {
    case UnexpectedEOF() => UnexpectedEOF()
    case UnexpectedChar(e, a) => {
      val (ee, aa) = k(e, a)
      UnexpectedChar(ee, aa)
    }
  }

// a ParseResult has one ParseError or some other things
def modifyParseError[A]:
  (ParseError => ParseError)
  => ParseResult[A]
  => ParseResult[A] =
  k => {
    case SuccessResult(a) => SuccessResult(a)
    case ErrorResult(e) => ErrorResult(k(e))
  }

def modifyParseResultActualChar[A]: (Char => Char) => ParseResult[A] => ParseResult[A] =
  modifyParseError compose modifyChars compose modifyActualChar
}

// A has exactly one B and some other things (lens)
// * F must be any functor
// * ~> must be Function1

// A has exactly one B or some other things (prism)
// * F must be any applicative
// * ~> must be any Choice

// A has exactly one B and/or some other things (traversal)
// * F must be any applicative
// * ~> must be Function1

// A has exactly one B and no other things (isomorphism)
// * F must be functor
// * ~> must be Profunctor

// A is B (and therefore A) (equality)
// optic
