let isPrime n =
    match n with
    | _ when n <= 1 -> false
    | 2 | 3 -> true
    | _ when n % 2 = 0 || n % 3 = 0 -> false
    | _ -> 
        let rec check i w = 
            match i * i > n with
            | true -> true
            | false -> 
                match n % i = 0 with
                | true -> false
                | false -> check (i + w) (6 - w)
        check 5 2

let digits = Set.ofList [1..9]

let generatePrimes =
    let rec generate current used remaining =
        seq {
            match current with
            | 0 -> ()
            | _ -> 
                let lastDigit = current % 10
                let validEnd = 
                    Set.count used = 1 && (lastDigit = 2 || lastDigit = 3 || lastDigit = 5 || lastDigit = 7) ||
                    Set.count used > 1 && (lastDigit = 1 || lastDigit = 3 || lastDigit = 7 || lastDigit = 9)
                match validEnd && isPrime current with
                | true -> yield (current, used)
                | false -> ()
            for d in remaining do
                yield! generate (current * 10 + d) (Set.add d used) (Set.remove d remaining)
        }
    generate 0 Set.empty digits

let countSets =
    let primes = generatePrimes |> Seq.toList |> List.sortByDescending (snd >> Set.count)
    let rec count index remaining =
        primes 
        |> List.skip index 
        |> List.mapi (fun i (n, d) -> (i + index, d))
        |> List.sumBy (fun (i, d) ->
            match Set.isSubset d remaining with
            | true -> 
                let newRemaining = Set.difference remaining d
                match Set.isEmpty newRemaining with
                | true -> 1
                | false -> count (i + 1) newRemaining
            | false -> 0
        )
    count 0 digits

printfn "%d" countSets
