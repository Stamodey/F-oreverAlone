// ----- task 19 -----
let rec gcd a b = 
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

let isCoprime a b =
    match gcd a b with
    | 1 -> true
    | _ -> false

let countCoprimeDigit number digit =
    match isCoprime digit number with
    | true -> 1
    | false -> 0

let rec countCoprimeDigits number accumulator =
    match number with
    | 0 -> accumulator
    | _ ->
        let digit, nextNumber = number % 10, number / 10
        let updatedAccumulator = accumulator + (countCoprimeDigit number digit)
        countCoprimeDigits nextNumber updatedAccumulator

let sumDigitDivisibleBy3 digit =
    match digit % 3 with
    | 0 -> digit
    | _ -> 0

let rec sumDigitsDivisibleBy3 number accumulator =
    match number with
    | 0 -> accumulator
    | _ ->
        let digit, nextNumber = number % 10, number / 10
        let updatedAccumulator = accumulator + (sumDigitDivisibleBy3 digit)
        sumDigitsDivisibleBy3 nextNumber updatedAccumulator

let findBestCoprimeDivisor candidateDivisor number maxDivisor maxCount =
    match number % candidateDivisor with
    | 0 ->
        let coprimeCount = countCoprimeDigits candidateDivisor 0
        if coprimeCount > maxCount then candidateDivisor, coprimeCount
        else maxDivisor, maxCount
    | _ -> maxDivisor, maxCount

let rec bestCoprimeDivisorHelper divisor number maxDivisor maxCount =
    match divisor with
    | 0 -> maxDivisor
    | _ ->
        let nextDivisor = divisor - 1
        let newMaxDivisor, newMaxCount = findBestCoprimeDivisor divisor number maxDivisor maxCount
        bestCoprimeDivisorHelper nextDivisor number newMaxDivisor newMaxCount

let bestCoprimeDivisor number =
    bestCoprimeDivisorHelper number number 1 0

let testNumber = 365

printfn "Количество цифр, взаимно простых с %d: %d" testNumber (countCoprimeDigits testNumber 0)
printfn "Сумма цифр %d, делящихся на 3: %d" testNumber (sumDigitsDivisibleBy3 testNumber 0)
printfn "Лучший взаимно простой делитель числа %d: %d" testNumber (bestCoprimeDivisor testNumber)
