module FMutant.Sample



let add a b = a + b
let isLessThanConst x = x > -9
let andOperator y z =  y && z
let alwaysTrue p = true || p
let division r s = r / s
let divisionByZero j = j / 0
let multiplication t k = t * k
let riskyDivision a = a / 1 + 0
let survivorFunction x = x / 1 + 0
let superWeakFunction x = x * 5