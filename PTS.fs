open System
open System.Text.RegularExpressions


type VehiclePassport(series: string, number: string, 
                    vin: string, manufacturer: string, 
                    model: string, year: int) as this =
    
    let seriesPattern = @"^\d{4}$"        // 4 цифры
    let numberPattern = @"^\d{6}$"        // 6 цифр
    let vinPattern = @"^[A-HJ-NPR-Z0-9]{17}$" // Стандартный VIN
    
    // Валидация при создании объекта
    do
        if not (Regex.IsMatch(series, seriesPattern)) then
            raise (ArgumentException("Неверный формат серии (требуется 4 цифры)"))
        if not (Regex.IsMatch(number, numberPattern)) then
            raise (ArgumentException("Неверный формат номера (требуется 6 цифр)"))
        if not (Regex.IsMatch(vin, vinPattern)) then
            raise (ArgumentException("Неверный формат VIN (17 символов: A-H, J-N, P, R-Z, 0-9)"))
        if year < 1900 || year > DateTime.Now.Year + 1 then
            raise (ArgumentException("Некорректный год выпуска"))

    member val Series = series
    member val Number = number
    member val VIN = vin
    member val Manufacturer = manufacturer
    member val Model = model
    member val ProductionYear = year

    override this.ToString() =
        $"ПТС: {this.Series} {this.Number}\n" +
        $"VIN: {this.VIN}\n" +
        $"Производитель: {this.Manufacturer}\n" +
        $"Модель: {this.Model}\n" +
        $"Год выпуска: {this.ProductionYear}"

    // Реализация сравнения
    interface IEquatable<VehiclePassport> with
        member this.Equals(other) =
            this.Series = other.Series && 
            this.Number = other.Number
            
    override this.Equals(obj) =
        match obj with
        | :? VehiclePassport as p -> (this :> IEquatable<_>).Equals(p)
        | _ -> false
        
    override this.GetHashCode() =
        hash (this.Series, this.Number)


try
    let pts1 = VehiclePassport(
        "1234", "567890", 
        "XWBBN9ZGXKJ000001", 
        "Toyota", 
        "Camry", 
        2020
    )
    
    let pts2 = VehiclePassport(
        "1234", "567890", 
        "Z94CB41AAGR000002", 
        "Hyundai", 
        "Solaris", 
        2022
    )
    
    let pts3 = VehiclePassport(
        "9876", "123456", 
        "VF7RD5FZ8EN123456", 
        "Renault", 
        "Logan", 
        2018
    )


    printfn "ПТС 1:\n%s\n" (pts1.ToString())
    printfn "ПТС 2:\n%s\n" (pts2.ToString())
    printfn "ПТС 3:\n%s\n" (pts3.ToString())


    printfn "ПТС1 == ПТС2: %b" (pts1 = pts2)
    printfn "ПТС1 == ПТС3: %b" (pts1 = pts3)

    
    // let invalidPts1 = VehiclePassport("12A4", "56B90", "1M8GDM9AXKP04258!", "Lada", "Granta", 2030)
with
| :? ArgumentException as ex -> printfn "Ошибка валидации: %s" ex.Message
