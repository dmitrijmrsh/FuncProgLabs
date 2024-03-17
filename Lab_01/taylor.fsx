//Вариант 16
let f (x : float) = (1.0 + 2.0 * x * x) * System.Math.Exp(x * x)

let a = 0.0
let b = 1.0
let n = 10
let eps = 0.000000001

let rec factorial (n : float) =
    if n = 0 || n = 1.0 then
        1.0
    else n * factorial(n - 1.0)

let rec pow (value : float) (exponent : float) =
    if exponent = 0 then
        1.0
    else
        value * (pow value (exponent - 1.0))

let dumb_taylor_series_member (x : float) (n : float) = (2.0 * n + 1.0) / (factorial n) * (pow x (2.0 * n))

let smart_taylor_series_member (x: float) (n : float) (prev : float) =
    if (n = 0) then
        dumb_taylor_series_member x n
    else
        ((x * x) / n) * ((2.0 * n + 1.0) / (2.0 * n - 1.0)) * prev


let rec dumb_taylor (x : float) (n : float) (acc : float) (eps : float) =
    let new_member = dumb_taylor_series_member x n
    if (new_member < eps) then
        (acc, n)
    else
        dumb_taylor x (n + 1.0) (acc + new_member) eps

let rec smart_taylor (x : float) (n : float) (acc : float) (prev : float) (eps : float) =
    let new_member = smart_taylor_series_member x n prev
    if (new_member < eps) then
        (acc, n)
    else
        smart_taylor x (n + 1.0) (acc + new_member) new_member eps
    
let main () =
    printfn "  x \t Builtin  Smart Taylor  #terms \t  Dumb Taylor \t #terms"
    for i=0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let (dumb_taylor_val, dumb_taylor_iter) = dumb_taylor x 0 0 eps
        let (smart_taylor_val, smart_taylor_iter) = smart_taylor x 0 0 0 eps
        printfn "%5.2f %11.6f %11.6f %7.0f %14.6f %7.0f" x (f x) smart_taylor_val smart_taylor_iter dumb_taylor_val dumb_taylor_iter

main()