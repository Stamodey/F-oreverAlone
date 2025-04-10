// 1.8 Найти индексы двух наименьших элементов массива
let task1_8_tail list =
    let rec findTwoSmallest list (min1Idx, min2Idx, min1, min2) idx =
        match list with
        | [] -> [min1Idx; min2Idx]
        | h :: t ->
            match h < min1, h < min2 with
            | true, _ -> findTwoSmallest t (idx, min1Idx, h, min1) (idx + 1)
            | false, true -> findTwoSmallest t (min1Idx, idx, min1, h) (idx + 1)
            | _ -> findTwoSmallest t (min1Idx, min2Idx, min1, min2) (idx + 1)
    let maxVal = System.Int32.MaxValue
    findTwoSmallest list (-1, -1, maxVal, maxVal) 0

let task1_8_church list =
    let maxVal = System.Int32.MaxValue
    let rec fold lst idx (min1Idx, min2Idx, min1, min2) =
        match lst with
        | [] -> [min1Idx; min2Idx]
        | h::t ->
            let newState =
                if h < min1 then (idx, min1Idx, h, min1)
                elif h < min2 then (min1Idx, idx, min1, h)
                else (min1Idx, min2Idx, min1, min2)
            fold t (idx + 1) newState
    fold list 0 (-1, -1, maxVal, maxVal)

// 1.18 Найти элементы перед первым минимальным
let task1_18_tail list =
    let rec findMin lst currentMin =
        match lst with
        | [] -> currentMin
        | h::t -> if h < currentMin then findMin t h else findMin t currentMin

    let rec findBefore lst minElem acc =
        match lst with
        | [] -> acc
        | h::t when h = minElem -> acc
        | h::t -> findBefore t minElem (h::acc)

    match list with
    | [] -> []
    | h::t ->
        let minElem = findMin t h
        findBefore list minElem [] |> List.rev

let task1_18_church list =
    let rec findMin lst minElem =
        match lst with
        | [] -> minElem
        | h::t -> if h < minElem then findMin t h else findMin t minElem

    let rec buildBefore lst minElem acc =
        match lst with
        | [] -> acc
        | h::t when h = minElem -> acc
        | h::t -> buildBefore t minElem (h::acc)

    match list with
    | [] -> []
    | h::t ->
        let minElem = findMin list h
        let result = buildBefore list minElem []
        let rec reverse l acc =
            match l with
            | [] -> acc
            | x::xs -> reverse xs (x::acc)
        reverse result []

// 1.28 Найти элементы между первым и последним максимальным
let task1_28_tail list =
    let rec findIndices lst idx firstIdx lastIdx =
        match lst with
        | [] -> (firstIdx, lastIdx)
        | h :: t ->
            let fIdx = if h = List.max list && firstIdx = -1 then idx else firstIdx
            let lIdx = if h = List.max list then idx else lastIdx
            findIndices t (idx + 1) fIdx lIdx

    let (firstIdx, lastIdx) = findIndices list 0 (-1) (-1)

    let rec takeBetween lst idx acc =
        match lst with
        | [] -> List.rev acc
        | h :: t when idx > firstIdx && idx < lastIdx -> takeBetween t (idx + 1) (h :: acc)
        | _ :: t -> takeBetween t (idx + 1) acc

    takeBetween list 0 []

let task1_28_church list =
    let maxElem = List.fold (fun acc x -> if x > acc then x else acc) System.Int32.MinValue list
    let rec findIndices lst idx firstIdx lastIdx =
        match lst with
        | [] -> (firstIdx, lastIdx)
        | h :: t ->
            let fIdx = if h = maxElem && firstIdx = -1 then idx else firstIdx
            let lIdx = if h = maxElem then idx else lastIdx
            findIndices t (idx + 1) fIdx lIdx

    let (firstIdx, lastIdx) = findIndices list 0 (-1) (-1)

    let rec takeBetween lst idx acc =
        match lst with
        | [] -> List.rev acc
        | h :: t when idx > firstIdx && idx < lastIdx -> takeBetween t (idx + 1) (h :: acc)
        | _ :: t -> takeBetween t (idx + 1) acc

    takeBetween list 0 []

// 1.38 Найти количество элементов, значение которых принадлежит отрезку [a..b]
let countInRange list a b =
    let rec countInRangeRec list a b acc =
        match list with
        | [] -> acc
        | h :: t ->
            let acc = if h >= a && h <= b then acc + 1 else acc
            countInRangeRec t a b acc
    countInRangeRec list a b 0

let task1_38_church list a b =
    let rec countInRangeRec lst acc =
        match lst with
        | [] -> acc
        | h :: t ->
            let acc = if h >= a && h <= b then acc + 1 else acc
            countInRangeRec t acc
    countInRangeRec list 0

// 1.48 Построить список с номерами элемента, который повторяется наибольшее число раз
let mostFrequentIndices list =
    let rec buildFrequencyList list mainList acc =
        match list with
        | [] -> acc
        | h :: t ->
            let freq = frequency mainList h 0
            let acc = acc @ [freq]
            buildFrequencyList t mainList acc
    let freqList = buildFrequencyList list list []
    let maxFreq = listMax freqList
    let rec getIndices list freqList maxFreq idx acc =
        match (list, freqList) with
        | ([], _) | (_, []) -> acc
        | (h :: t1, f :: t2) ->
            let acc = if f = maxFreq then acc @ [idx] else acc
            getIndices t1 t2 maxFreq (idx + 1) acc
    getIndices list freqList maxFreq 0 []

let task1_48_church list =
    let rec buildFrequencyList lst mainList acc =
        match lst with
        | [] -> acc
        | h :: t ->
            let freq = List.fold (fun acc x -> if x = h then acc + 1 else acc) 0 mainList
            buildFrequencyList t mainList (acc @ [freq])

    let freqList = buildFrequencyList list list []
    let maxFreq = List.fold (fun acc x -> if x > acc then x else acc) 0 freqList
    let rec getIndices lst freqList maxFreq idx acc =
        match lst, freqList with
        | [], _ | _, [] -> acc
        | h :: t1, f :: t2 ->
            let acc = if f = maxFreq then acc @ [idx] else acc
            getIndices t1 t2 maxFreq (idx + 1) acc
    getIndices list freqList maxFreq 0 []

// 1.58 Вывести количество элементов, которые могут быть получены как сумма двух любых других элементов списка
let task1_58_tail list =
    let rec getSums lst acc =
        match lst with
        | [] -> acc
        | h::t -> 
            let rec sumRest rest acc =
                match rest with
                | [] -> acc
                | x::xs -> 
                    if h + x <> x && List.contains (h + x) list then sumRest xs (acc + 1)
                    else sumRest xs acc
            getSums t (acc + sumRest t 0)
    let sums = getSums list 0
    sums


let task1_58_church list =
    let rec getSums lst acc =
        match lst with
        | [] -> acc
        | h::t ->
            let rec sumRest rest acc =
                match rest with
                | [] -> acc
                | x::xs -> 
                    if h + x <> x && List.contains (h + x) list then sumRest xs (acc + 1)
                    else sumRest xs acc
            getSums t (acc + sumRest t 0)

    let sums = getSums list 0
    sums

