open System
open System.IO

let samplePath = Path.Combine (__SOURCE_DIRECTORY__,"../sample")
let sample = File.ReadAllLines samplePath
