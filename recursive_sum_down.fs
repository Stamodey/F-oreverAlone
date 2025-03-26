open System

let sumOfDigits num =
    let rec sumOfDigitsRec num currentSum =
        if num = 0 then currentSum
        else 
            sumOfDigitsRec (num / 10) (currentSum + (num % 10))

    sumOfDigitsRec num 0

[<EntryPoint>]
let main avrg = 
    printf "Введите число: "
    let num = int (Console.ReadLine())

    printfn "Сумма цифр числа %d" (sumOfDigits num)
    0
