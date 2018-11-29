module Program

open System
open Helpers

type TriangleType =
| Incorrect
| Isosceles
| Equilateral
| Rectangular
| Wrong

let rec getTriangleSide (side:string) = 
    side |> toDouble (fun v -> v > 0.0)
                     (fun () -> sprintf "Invalid side length: %s" side)

let rec getTriangleAngle (angle:string) = 
    angle |> toDouble (fun v -> v > 0.0 && v < 180.0)
                      (fun () -> sprintf "Invalid angle: %s" angle)

let getTriangleType a alpha beta = 
    let gamma = 180.0 - (alpha + beta) 

    let b = a * Math.Sin(alpha * 180.0 / Math.PI) / Math.Sin(gamma * 180.0 / Math.PI)
    let c = a * Math.Sin(beta * 180.0 / Math.PI) / Math.Sin(gamma * 180.0 / Math.PI)

    if a + b <= c || a + c <= b || b + c <= c then 
        Incorrect
    else if alpha = 90.0 || beta = 90.0 || alpha + beta = 90.0 then 
        Rectangular
    else if a = b && b = c then 
        Equilateral
    else if a = b || b = c || a = c then 
        Isosceles 
    else Wrong

[<EntryPoint>]
let main argv =
    printfn "Name: Andrii Rublov\nGroup: PZPI-16-3"
    let tryItemFromArgv = tryItemResult argv
    either {
        let! sideInput = tryItemFromArgv 0 "First argument (side) is not found"
        let! angleAInput = tryItemFromArgv 1 "Second argument (first angle) is not found"
        let! angleBInput = tryItemFromArgv 2 "Third argument (second angle) is not found"
        let! side = sideInput |> getTriangleSide
        let! firstAngle = angleAInput |> getTriangleAngle
        let! secondAngle = angleBInput |> getTriangleAngle
        return getTriangleType side firstAngle secondAngle
    } 
    |> function
    | Success s -> s |> printfn "Triangle type: %A"
    | Failure f -> f |> printfn "Error: %s"

    0
