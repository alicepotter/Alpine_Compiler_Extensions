let x = 5
fun f() -> Int {
  match x {
    case 5 then 5
    case 2 then 2
  }
}
let main = print(f())

