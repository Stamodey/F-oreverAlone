// ----- task 6 -----
let chooseFunction condition = 
    match condition with 
    | true -> (fun num ->
        let rec sumOfDigitsRec num currentSum =
            match num with
            | 0 -> currentSum
            | _ -> sumOfDigitsRec (num /10) (currentSum + (num % 10))
        sumOfDigitsRec num 0)
    
    | false -> (fun num ->
        let rec factorialRec num acc = 
            match num with
            | 0 -> acc
            | _ -> factorialRec (num - 1) (num * acc)
        
        factorialRec num 1)

let sumFunction = chooseFunction true
let factorialFunction = chooseFunction false

printfn "Сумма цифр: %d" (sumFunction 1234)
printfn "Факториал: %d" (factorialFunction 5)

// ----- task 7 - 8 -----
let rec foldDigits num func acc =
    match num with
    | 0 -> acc
    | _ -> foldDigits (num / 10) func (func acc (num % 10))

let processDigits numbers operetions initial = 
    let rec loop num acc = 
        match num with
        | 0 -> acc
        | _ ->
            let digit = num % 10
            loop (num / 10) (operetions digit acc)
    loop numbers initial

printfn "Сумма цифр: %d" (processDigits 711 (fun x y -> x + y) 0)
printfn "Произведение цифр: %d" (processDigits 271 (fun x y -> x * y) 1)
printfn "Максимальная цифра: %d" (processDigits 711 max 0)
printfn "Минимальная цифра: %d" (processDigits 711 min 9)

// ----- task 9 - 10-----
let processDigitsWithCondition number operation initial condition =
    let rec loop num acc =
        match num with
        | 0 -> acc
        | _ ->
            match condition (num % 10) with
            | true -> loop (num / 10) (operation (num % 10) acc)
            | false -> loop (num / 10) acc
    
    loop number initial

printfn "Сумма чётных цифр: %d" (processDigitsWithCondition 7112 (fun x y -> x + y) 0 (fun x -> x % 2 = 0))
printfn "Произведение нечётных цифр: %d" (processDigitsWithCondition 2713 (fun x y -> x * y) 1 (fun x -> x % 2 <> 0))
printfn "Максимальная чётная цифра: %d" (processDigitsWithCondition 71128 max 0 (fun x -> x % 2 = 0))
printfn "Минимальная нечётная цифра: %d" (processDigitsWithCondition 71139 min 9 (fun x -> x % 2 <> 0))
