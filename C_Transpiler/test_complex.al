let x = #record("fiel", 3)
let y = x.0
let z = x.1
let a =match y {
  case "bonjour" then 1
    case "fiel" then z
    case "salut" then 3
}
let main = print(a + 4)