open System
open System.Windows.Forms

let calculateAverage (numbers: int list) =
    if List.isEmpty numbers then
        0.0
    else
        numbers |> List.map float |> List.average

let form = new Form(Text = "Среднее значение чисел", Width = 400, Height = 200)

let inputBox = new TextBox(Left = 20, Top = 20, Width = 340)

let calculateButton = new Button(Text = "Вычислить среднее", Left = 20, Top = 60, Width = 150)

let resultLabel = new Label(Left = 20, Top = 100, Width = 340, Height = 40)

calculateButton.Click.Add(fun _ ->
    try
        let input = inputBox.Text

        let numbers =
            input.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Array.toList

        let average = calculateAverage numbers
        resultLabel.Text <- "Среднее значение: " + average.ToString("F2")
    with
    | _ -> resultLabel.Text <- "Ошибка ввода! Введите числа через пробел или запятую."
)


form.Controls.Add(inputBox)
form.Controls.Add(calculateButton)
form.Controls.Add(resultLabel)

[<STAThread>]
Application.EnableVisualStyles()
Application.Run(form)
