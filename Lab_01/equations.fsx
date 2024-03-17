//Уравнения 16-18
let eps = 0.00001

let secion_center (a : float) (b : float) = (a + b) / 2.0

let rec pow (value : float) (exponent : float) =
    if exponent = 0 then
        1.0
    else
        value * (pow value (exponent - 1.0))

let rec dichotomy (f : float -> float) (a : float) (b : float) (eps : float) =
    let c = secion_center a b
    if abs (b - a) < eps then
        c
    else
        if (f a) * (f c) < 0 then
            dichotomy f a c eps
        elif (f b * f c < 0) then
            dichotomy f c b eps
        else
            printfn "The root is not found"
            c

let rec iterations (phi : float -> float) (x0 : float) (eps : float) =
    let x1 = phi x0
    if (abs (x1 - x0) < eps) then
        x1
    else
        iterations phi x1 eps

let rec newton (f : float -> float) (f' : float -> float) (x0 : float) (eps : float) =
    let phi x = x - (f x / f' x)
    iterations phi x0 eps


let f1 (x : float) = 3.0 * System.Math.Sin(x ** 0.5) + 0.35 * x - 3.8
let f2 (x : float) = 0.25 * (pow x 3) + x - 1.2502
let f3 (x : float) = x + (x ** 0.5) + (x ** (1.0 / 3.0)) - 2.5

let phi1 (x : float) = (3.8 / 0.35) - (3.0 / 0.35) * System.Math.Sin(x ** 0.5)
let phi2 (x : float) = 1.2502 - 0.25 * x * x * x
let phi3 (x : float) = 2.5 - (x ** 0.5) - (x ** (1.0 / 3.0))

let f1' (x : float) = 3.0 * System.Math.Cos(x ** 0.5) * (1.0 / (2.0 * (x ** 0.5))) + 0.35
let f2' (x : float) = 0.75 * x * x + 1.0
let f3' (x : float) = 1.0 + 1.0 / (2.0 * (x ** 0.5)) + 1.0 / (3.0 * (x ** (2.0 / 3.0))) 

let main() =
    printfn "\t Dichotomy \t Iterations \t Newton"
    printfn "f1\t %.5f \t %.5f \t %.5f" (dichotomy f1 2.0 3.0 eps) (iterations phi1 (secion_center 2.0 3.0) eps) (newton f1 f1' (secion_center 2.0 3.0) eps)
    printfn "f2\t %.5f \t %.5f \t %.5f" (dichotomy f2 0 2.0 eps) ((iterations phi2 (secion_center 0 2.0) eps)) ((newton f2 f2' (secion_center 0 2.0) eps))
    printfn "f3\t %.5f \t %.5f \t %.5f" (dichotomy f3 0.4 1.0 eps) (iterations phi3 (secion_center 0.4 1.0) eps) (newton f3 f3' (secion_center 0.4 1.0) eps)

main()