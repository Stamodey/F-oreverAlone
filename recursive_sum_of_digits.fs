open System

let rec sumOfDigits num =
    if num = 0 then 
        0
    else 
        (num % 10) + sumOfDigits (num / 10)

[<EntryPoint>]
let main avrg = 
    printf "Введите число: " 
    let num = int (Console.ReadLine())

    printfn "Сумма цифр числа: %d" (sumOfDigits num)
    0
