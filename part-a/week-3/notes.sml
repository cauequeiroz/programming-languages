(* Records *)
val student = {
  name = "Caue Queiroz",
  age = 16,
  brazilian = true
}

val student_name = #name student
val student_age = #age student
val student_brazilian = #brazilian student

(* Datatype *)
datatype flavours = Chocolate
                  | Vanilla
                  | DarkChocolate

datatype id = NumericId of int
            | StudentName of string
            | None 

datatype exp = Constant of int
             | Add of exp * exp
             | Multiply of exp * exp
             | Negate of exp


(* Pattern Maching *)
fun isFlavourAvailable (f: flavours) =
  case f of Chocolate => true
          | DarkChocolate => false
          | Vanilla => true

val isFlavourAvailable_test1 = isFlavourAvailable(Chocolate) = true
val isFlavourAvailable_test2 = isFlavourAvailable(DarkChocolate) = false
val isFlavourAvailable_test3 = isFlavourAvailable(Vanilla) = true


fun isStudentAllowed(i: id) =
  case i of
    None => false
  | NumericId n => n = 42
  | StudentName s => s = "Douglas Adams"

val isStudentAllowed_test1 = isStudentAllowed(None) = false
val isStudentAllowed_test2 = isStudentAllowed(NumericId 42) = true
val isStudentAllowed_test3 = isStudentAllowed(NumericId 56) = false
val isStudentAllowed_test4 = isStudentAllowed(StudentName "Caue Queiroz") = false
val isStudentAllowed_test5 = isStudentAllowed(StudentName "Douglas Adams") = true


fun howManyAdd (e: exp) =
  case e of
    Constant i => 0
  | Add (exp1, exp2) => 1 + howManyAdd(exp1) + howManyAdd(exp2)
  | Multiply (exp1, exp2) => howManyAdd(exp1) + howManyAdd(exp2)
  | Negate exp1 => howManyAdd(exp1)


val howManyAdd_test1 = howManyAdd(Constant 2) = 0
val howManyAdd_test2 = howManyAdd(Add (Constant 1, Constant 2)) = 1
val howManyAdd_test3 = howManyAdd(Negate (Constant 4)) = 0
val howManyAdd_test4 = howManyAdd(Negate (Add (Constant 1, Constant 2))) = 1
val howManyAdd_test5 = howManyAdd(Negate (Add (Add (Constant 1, Constant 2), Constant 2))) = 2


fun sumAllItems numbers =
  case numbers of
    [] => 0
  | first::rest => first + sumAllItems rest

fun appendNumbers (ns1, ns2) =
  case ns1 of
    [] => ns2
  | first::rest => first::appendNumbers(rest, ns2)

fun sumTupple (a, b, c) = a + b + c 

fun fullName {first=x, last=y} = x ^ " " ^ y
fun fullName2 person = let val {first=x, last=y} = person in x ^ " " ^ y end
