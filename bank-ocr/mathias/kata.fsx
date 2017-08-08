(*
TODO
? what if the input is not well-formed
*)

open System
open System.IO

type BlockSize = {
    Width:int
    Height:int
    }

let prepare (size:BlockSize) (blocks:int) (lines:string[]) = 
    Array2D.init 
        size.Height 
        (size.Width * blocks) 
        (fun row col -> lines.[row].[col])    

let blockAt (size:BlockSize) (data:_[,]) position =
    
    let offset = size.Width * position
    Array2D.init 
        size.Height size.Width 
        (fun row col -> data.[row, col + offset])

let blockPrinter (size:BlockSize) (block:_[,]) =
    printfn ""
    block 
    |> Array2D.iteri (fun row col x -> 
        // linebreaks
        if col % (size.Width) = 0 then printfn ""
        printf "%A" x)

let size = {
    Width = 3
    Height = 4
    }

let samplePath = Path.Combine (__SOURCE_DIRECTORY__,"../sample")
let sample = File.ReadAllLines samplePath
let prepared = prepare size 9 sample

let blockReader = blockAt size prepared

[ 0 .. 8 ]
|> Seq.map blockReader
|> Seq.iter (blockPrinter size)
