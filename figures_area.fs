type ShapeDU =
    | Rectangle of width: float * height: float
    | Square of side: float
    | Circle of radius: float

module ShapeDU =
    let area =
        function
        | Rectangle (w, h) -> w * h
        | Square s -> s * s
        | Circle r -> System.Math.PI * r * r

    let description =
        function
        | Rectangle (w, h) ->
            $"Прямоугольник: ширина = {w:F2}, высота = {h:F2}, площадь = {w * h:F2}"
        | Square s ->
            $"Квадрат: сторона = {s:F2}, площадь = {s * s:F2}"
        | Circle r ->
            let a = System.Math.PI * r * r
            $"Круг: радиус = {r:F2}, площадь = {a:F2}"

let shapes: ShapeDU list = [
    Rectangle(4.0, 5.0)
    Square(3.0)
    Circle(2.0)
]

let printShape shape =
    shape
    |> ShapeDU.description
    |> printfn "%s"

shapes
|> List.iter printShape
