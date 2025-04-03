open System


let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)


let isPrime x =
    match x > 1 with
    | false -> false
    | true -> seq { 2..int (sqrt (float x)) } |> Seq.forall (fun d -> x % d <> 0)


let sumDigits n =
    abs n |> string |> Seq.sumBy (fun c -> int c - int '0')

let processDivisors n initValue operation predicate =
    [1..abs n] |> List.fold (fun acc d -> 
        match n % d = 0 && predicate d with
        | true -> operation acc d
        | false -> acc
    ) initValue

let sumPrimeDivisors n =
    processDivisors n 0 (fun acc d -> match isPrime d with
                                       | true -> acc + d
                                       | false -> acc) isPrime

let countOddDigitsOver3 n =
    abs n |> string |> Seq.filter (fun c -> let d = int c - int '0' in d % 2 <> 0 && d > 3) |> Seq.length

let productSpecialDivisors n =
    let targetSum = sumDigits n
    match n with
    | 0 -> 0
    | _ -> processDivisors n 1 (fun acc d -> 
            match sumDigits d < targetSum with
            | true -> acc * d
            | false -> acc) (fun _ -> true)

let getOperation = function
    | 1 -> sumPrimeDivisors
    | 2 -> countOddDigitsOver3
    | 3 -> productSpecialDivisors
    | _ -> fun _ -> 0

let parseInput (input: string) =
    let parts = input.Split()
    (int parts.[0], int parts.[1])


let executeOperation () =
    Console.ReadLine()
    |> parseInput
    |> fun (opId, arg) -> getOperation opId arg
    |> printfn "Результат: %d"

[<EntryPoint>]
let main argv =
    executeOperation ()
    0
