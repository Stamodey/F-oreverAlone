open System

let maxAsciiCode (s: string) =
    s.ToCharArray() |> Array.map int |> Array.max


let quadraticDeviation (s: string) =
    let maxCode = maxAsciiCode s
    let n = s.Length

    let deviations =
        List.init (n / 2) (fun i -> 
            let diff = abs ((int s.[i]) - (int s.[n - 1 - i]))
            (float maxCode - float diff) ** 2.0)
    deviations |> List.sum

let countMirrorTriples (s: string) =
    let n = s.Length

    let mirrorTriples =
        List.init (n - 2) (fun i -> 
            if s.[i] = s.[i + 2] then 1 else 0)
    let totalCount = List.sum mirrorTriples
    if n < 3 then 0.0 else float totalCount / float (n - 2)

let sortStrings (strings: string list) =
    strings
    |> List.sortBy (fun s -> (quadraticDeviation s, countMirrorTriples s))

[<EntryPoint>]
let main argv =

    printfn "Введите строки, разделенные переносом строки. Для завершения ввода введите пустую строку:"
    let rec readStrings acc =
        let input = Console.ReadLine()
        if input = "" then acc else readStrings (acc @ [input])
    let strings = readStrings []

    let sortedStrings = sortStrings strings

    printfn "Отсортированные строки:"
    sortedStrings |> List.iter (printfn "%s")

    0 
