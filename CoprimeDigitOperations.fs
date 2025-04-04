// ----- task 11 - 12 -----
let favoriteLanguage language = 
    match language with
    | "F#" | "Prolog" -> "Ты - подлиза ~_~"
    | "Python" -> "Выбор большинства ¬_¬"
    | "Rust" | "Haskell" -> "Скрываешься от систем статического анализа ^_-"
    | "C" | "C++" -> "Ок, дед;"
    | "JS" -> "Понял, ты вебщик"
    | _ -> "Интересный выбор"

let main_superposition () =
    printf "Какой твой любимый язык программирования: "
    let userInput = System.Console.ReadLine()
    let result = (printf "%s") << favoriteLanguage
    result userInput

let main_currying () =
    let askFavoriteLanguage () =
        printf "Какой твой любимый язык программирования: "
        System.Console.ReadLine()
    
    let displayResult result =
        printf "%s" result

    let curriedFunction = favoriteLanguage >> displayResult
    curriedFunction (askFavoriteLanguage ())

main_superposition ()

main_currying ()

// ----- task 13 - 14 -----
let rec gcd a b = 
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

let isCoprime a b =
    match gcd a b with
    | 1 -> true
    | _ -> false

let processCoprimeDigits number operation init =
    let rec step num acc =
        match num with
        | 0 -> acc
        | _ -> 
            let digit = num % 10
            let nextNum = num / 10
            let newAcc = 
                match isCoprime digit number with
                | true -> operation acc digit
                | false -> acc
            step nextNum newAcc

    step number init

let eulerTotient n =
    let rec countCoprimes i acc =
        match i with
        | 0 -> acc
        | _ ->
            let newAcc = 
                match isCoprime i n with
                | true -> acc + 1
                | false -> acc
            countCoprimes (i - 1) newAcc

    countCoprimes n 0

System.Console.WriteLine(processCoprimeDigits 365 (+) 0)
System.Console.WriteLine(processCoprimeDigits 271828 ( * ) 1) 
System.Console.WriteLine(processCoprimeDigits 271828 min 9)
System.Console.WriteLine(processCoprimeDigits 271828 max 0)

System.Console.WriteLine(eulerTotient 9)
System.Console.WriteLine(eulerTotient 10)
System.Console.WriteLine(eulerTotient 15)

// ----- task 15 -----
let processFilteredCoprimeDigits number operation initialValue condition =
    let rec process remainingNumber result =
        match remainingNumber with
        | 0 -> result
        | _ ->
            let digit, nextNumber = remainingNumber % 10, remainingNumber / 10
            let updatedResult =
                match isCoprime digit number && condition digit with
                | true -> operation result digit
                | false -> result
            process nextNumber updatedResult

    process number initialValue

printfn "Сумма цифр, взаимно простых с 365 и > 3: %d" 
    (processFilteredCoprimeDigits 365 (+) 0 (fun x -> x > 3))

printfn "Произведение цифр, взаимно простых с 271828 и > 1: %d" 
    (processFilteredCoprimeDigits 271828 (*) 1 (fun x -> x > 1))

// ----- task 16  -----
let countCoprimeDigits number =
    let rec count remainingNumber accumulator =
        match remainingNumber with
        | 0 -> accumulator
        | _ ->
            let digit, nextNumber = remainingNumber % 10, remainingNumber / 10
            let updatedAccumulator = accumulator + (if isCoprime digit number then 1 else 0)
            count nextNumber updatedAccumulator
    count number 0

let sumDigitsDivisibleBy3 number =
    let rec sum remainingNumber accumulator =
        match remainingNumber with
        | 0 -> accumulator
        | _ ->
            let digit, nextNumber = remainingNumber % 10, remainingNumber / 10
            let updatedAccumulator = accumulator + (if digit % 3 = 0 then digit else 0)
            sum nextNumber updatedAccumulator
    sum number 0

let bestCoprimeDivisor number =
    let rec findDivisors divisor maxDivisor maxCount =
        match divisor with
        | 0 -> maxDivisor
        | _ ->
            let nextDivisor = divisor - 1
            let updatedMax =
                match number % divisor with
                | 0 ->
                    let count = countCoprimeDigits divisor
                    if count > maxCount then (divisor, count) else (maxDivisor, maxCount)
                | _ -> (maxDivisor, maxCount)
            findDivisors nextDivisor (fst updatedMax) (snd updatedMax)
    findDivisors number 1 0

let testNumber = 365

printfn "Количество цифр, взаимно простых с %d: %d" testNumber (countCoprimeDigits testNumber)
printfn "Сумма цифр %d, делящихся на 3: %d" testNumber (sumDigitsDivisibleBy3 testNumber)
printfn "Лучший взаимно простой делитель числа %d: %d" testNumber (bestCoprimeDivisor testNumber)
