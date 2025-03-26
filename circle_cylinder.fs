open System

[<EntryPoint>]
let main avrg =
    printf "Введите радиус круга: "
    let radius = float (Console.ReadLine())

    printf "Введите высоту цилиндра: "
    let height = float (Console.ReadLine()) 

    let areaOfCircle radius =
        System.Math.PI * radius * radius

    let volumeOfCylinder radius height =
        areaOfCircle radius * height
    
    printfn "С использованием суперпозиции:"
    printfn "Площадь круга: %f" (areaOfCircle radius)
    printfn "Объем цилиндра: %f" (volumeOfCylinder radius height)
    
    let areaOfCircleC = 
        fun radius -> radius * radius * System.Math.PI

    let volumeOfCylinderC = 
        fun radius -> fun height -> areaOfCircleC radius * height

    printfn "\nС использованием каррирования:"
    printfn "Площадь круга: %f" (areaOfCircleC radius)
    printfn "Объем цилиндра: %f" ((volumeOfCylinderC radius) height)

    0
