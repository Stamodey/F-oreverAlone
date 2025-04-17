open FParsec

// Тип выражений
type Expr =
    | Number of float
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

// Парсеры
let numberParser = pfloat |>> Number
let ws = spaces

let operatorParser op =
    between ws ws (skipChar op) |>> (fun () -> op)

let expr, exprRef = createParserForwardedToRef()

let term = 
    numberParser <|>
    between (skipChar '(' >>. ws) (ws >>. skipChar ')') expr

let mulDivOp = operatorParser '*' <|> operatorParser '/'
let addSubOp = operatorParser '+' <|> operatorParser '-'

let mulDiv = 
    term .>>. many (mulDivOp .>>. term)
    |>> fun (first, rest) ->
        rest |> List.fold (fun acc (op, term) ->
            match op with
            | '*' -> Mul(acc, term)
            | '/' -> Div(acc, term)
            | _ -> acc
        ) first

let addSub = 
    mulDiv .>>. many (addSubOp .>>. mulDiv)
    |>> fun (first, rest) ->
        rest |> List.fold (fun acc (op, term) ->
            match op with
            | '+' -> Add(acc, term)
            | '-' -> Sub(acc, term)
            | _ -> acc
        ) first

do exprRef := addSub


let fullParser = ws >>. expr .>> ws .>> eof

let parseExpression str =
    match run fullParser str with
    | Success(result, _, _) -> Some result
    | Failure(errorMsg, _, _) -> 
        printfn "Ошибка парсинга: %s" errorMsg
        None

let rec eval = function
    | Number n -> n
    | Add(a, b) -> eval a + eval b
    | Sub(a, b) -> eval a - eval b
    | Mul(a, b) -> eval a * eval b
    | Div(a, b) -> eval a / eval b

let testCases = [
    "2+3*4"
    "(2+3)*4"
    "3.5*(4-2/1)"
    "10/(2+3)"
]

testCases |> List.iter (fun test ->
    printfn "\nПарсинг выражения: %s" test
    match parseExpression test with
    | Some expr ->
        printfn "Структура: %A" expr
        printfn "Результат: %.2f" (eval expr)
    | None -> ()
)
