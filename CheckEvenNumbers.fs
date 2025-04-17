open System
open System.Windows.Forms

let allEven (numbers: int list) =
    List.forall (fun x -> x % 2 = 0) numbers

let form = new Form(Text = "Проверка чётных чисел", Width = 400, Height = 200)

let inputBox = new TextBox(Left = 20, Top = 20, Width = 340)

let checkButton = new Button(Text = "Проверить", Left = 20, Top = 60, Width = 100)

let resultLabel = new Label(Left = 20, Top = 100, Width = 340, Height = 40)

checkButton.Click.Add(fun _ ->
    try
        let input = inputBox.Text
        let numbers =
            input.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Array.toList

        if allEven numbers then
            resultLabel.Text <- "Все числа чётные"
        else
            resultLabel.Text <- "Не все числа чётные"
    with
    | _ -> resultLabel.Text <- "Ошибка ввода! Введите числа через пробел или запятую."
)

form.Controls.Add(inputBox)
form.Controls.Add(checkButton)
form.Controls.Add(resultLabel)


[<STAThread>]
Application.EnableVisualStyles()
Application.Run(form)
