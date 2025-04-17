open System

[<AbstractClass>]
type Shape() =
    abstract member Area: float
    abstract member Description: string
    override this.ToString() = this.Description

type IPrint =
    abstract member Print: unit -> unit

type Rectangle(width: float, height: float) =
    inherit Shape()
    member _.Width = width
    member _.Height = height
    override _.Area = width * height
    override _.Description =
        $"Прямоугольник: ширина = {width:F2}, высота = {height:F2}, площадь = {width * height:F2}"
    interface IPrint with
        member this.Print() = Console.WriteLine(this.ToString())

type Square(side: float) =
    inherit Rectangle(side, side)
    override _.Description =
        $"Квадрат: сторона = {side:F2}, площадь = {side * side:F2}"

type Circle(radius: float) =
    inherit Shape()
    member _.Radius = radius
    override _.Area = Math.PI * radius * radius
    override _.Description =
        $"Круг: радиус = {radius:F2}, площадь = {Math.PI * radius * radius:F2}"
    interface IPrint with
        member this.Print() = Console.WriteLine(this.ToString())

[<Literal>]
let separator = "-----------------------------"

let printShapes (shapes: IPrint list) =
    shapes
    |> List.iter (fun shape ->
        shape.Print()
        Console.WriteLine(separator))

[ Rectangle(4.0, 5.0) :> IPrint
  Square(3.0) :> IPrint
  Circle(2.0) :> IPrint ]
|> printShapes
