open System

let readArray () =
    printfn "Введите элементы массива через пробел (в порядке неубывания):"
    Console.ReadLine().Split(' ') |> Array.map int

let symmetricDifference (array1: int[]) (array2: int[]) =
    let set1 = Set.ofArray array1
    let set2 = Set.ofArray array2
    Set.difference set1 set2 |> Set.union (Set.difference set2 set1) |> Set.toArray

[<EntryPoint>]
let main argv =
    printfn "Введите первый массив:"
    let array1 = readArray ()
    printfn "Введите второй массив:"
    let array2 = readArray ()

    let result = symmetricDifference array1 array2

    printfn "Симметрическая разность массивов:"
    result |> Array.iter (printf "%d ")
    printfn ""

    0
