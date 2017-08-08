System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

open System.IO

module ``7SegParser`` = 
    type Digit = char [] []
    [<AutoOpen>]
    module private _Impl = 
        let digitReference = 
            [|"    _  _     _  _  _  _  _ " 
              "  | _| _||_||_ |_   ||_||_|"
              "  ||_  _|  | _||_|  ||_| _|"
              "                           "|]
        let parseDigit (raw:Digit array) chunk= 
            [|raw.[0].[chunk]
              raw.[1].[chunk]
              raw.[2].[chunk]
              raw.[3].[chunk]|]

        let parse7segDigits (asciiRows:string array) = 
            //let sample = File.ReadLines(fileName)
            let totalLength = asciiRows.[0].Length / 3
            let digits =
                asciiRows
                |> Array.map (Seq.chunkBySize 3 >> Seq.toArray)
            [|0..totalLength-1|]
            |> Array.map (parseDigit digits)

    let readFileToDigits fileName =
        File.ReadLines(fileName)
        |> Seq.toArray
        |> parse7segDigits
    
    let digitToInt = 
        let lookup = 
            digitReference
            |> parse7segDigits
            |> Seq.mapi (fun i digit -> digit,i+1)
            |> Map.ofSeq
        fun digit -> lookup |> Map.tryFind digit


// read a file and print the parsed seven-segment numbers
"bank-ocr/sample" // file name
|> ``7SegParser``.readFileToDigits
|> Seq.map ``7SegParser``.digitToInt
|> Seq.iter (function
    | Some digit -> printf "%d;" digit
    | None -> printf "N/A;")
                        