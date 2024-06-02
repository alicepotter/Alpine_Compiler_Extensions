let y = #r(1 + 3, "ss")
let x = match y {
    case #r(4, "sss") then 1
    case _ then 0
}
let main = print(x)