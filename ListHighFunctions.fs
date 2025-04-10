open System

let allDigits lst =
    lst
    |> List.collect (fun n -> n.ToString() |> Seq.map (fun c -> int c - int '0') |> List.ofSeq)

let buildFreq digits =
    digits
    |> List.groupBy id
    |> List.map (fun (digit, group) -> digit, List.length group)
    |> Map.ofList

let frequentDigits freqMap =
    let maxFreq = freqMap |> Map.toList |> List.map snd |> List.max
    freqMap
    |> Map.toList
    |> List.filter (fun (_, count) -> count > maxFreq / 2)
    |> List.map fst
    |> Set.ofList

let avgFrequentDigits (num: int) (frequent: Set<int>) =
    let digits =
        num.ToString()
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.filter (fun d -> Set.contains d frequent)
        |> Seq.toList
    match digits with
    | [] -> 0.0
    | _ ->
        digits
        |> List.averageBy float

let solve (lst: int list) : float list =
    let digits = allDigits lst
    let freqMap = buildFreq digits
    let frequent = frequentDigits freqMap
    lst |> List.map (fun n -> avgFrequentDigits n frequent)

[<EntryPoint>]
let main argv =
    printfn "Введите числа через пробел:"
    let input = Console.ReadLine()
    let numbers =
        input.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Array.toList

    let result = solve numbers

    printfn "Результат:"
    result |> List.iteri (fun i v -> printfn "Число %d -> %.2f" (numbers.[i]) v)

    0
