open System
open System.IO

let samplePath = Path.Combine (__SOURCE_DIRECTORY__,"../sample")
let sample = File.ReadAllLines samplePath

let prepare (lines:string[]) = 
    Array2D.init 4 (3*9) (fun row col -> lines.[row].[col])
    
let prepared = prepare sample

let width = 3
let height = 4

let blockAt (data:_[,]) position =
    
    let offset = width * position
    Array2D.init height width (fun row col -> data.[row,col+offset])

[ 0 .. 9 ]
|> Seq.map (blockAt prepared)
