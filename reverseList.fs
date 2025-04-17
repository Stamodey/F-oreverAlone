open System
open System.Windows.Forms

let reverseList (numbers: int list) =
    List.rev numbers

let form = new Form(Text = "Зеркальное отображение списка", Width = 400, Height = 200)

let inputBox = new TextBox(Left = 20, Top = 20, Width = 340)

let reverseButton = new Button(Text = "Отобразить зеркально", Left = 20, Top = 60, Width = 150)

let resultLabel = new Label(Left = 20, Top = 100, Width = 340, Height = 40)


reverseButton.Click.Add(fun _ ->
    try
        let input = inputBox.Text

        let numbers =
            input.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Array.toList


        let reversed = reverseList numbers
        resultLabel.Text <- "Зеркальный список: " + String.Join(", ", reversed)
    with
    | _ -> resultLabel.Text <- "Ошибка ввода! Введите числа через пробел или запятую."
)

form.Controls.Add(inputBox)
form.Controls.Add(reverseButton)
form.Controls.Add(resultLabel)

[<STAThread>]
Application.EnableVisualStyles()
Application.Run(form)
