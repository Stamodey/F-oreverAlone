open System

let countEvenLengthWords (input: string) =
    input.Split(' ')
    |> Array.filter (fun word -> word.Length % 2 = 0)
    |> Array.length

[<EntryPoint>]
let main argv =

    printfn "Введите строку:"
    let inputString = Console.ReadLine()

    let result = countEvenLengthWords inputString

    printfn "Количество слов с четным количеством символов: %d" result

    0 
