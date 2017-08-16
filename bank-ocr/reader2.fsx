open System
open System.IO

/////////////////////////////////////////////////////////////////////////////
type OcrDigit = 
    | Digit of int * int array
    | Ambiguous of int array

let isDigit od = 
    match od with
    | Digit(_) -> true
    | _ -> false

let isUnambiguousDigit od = 
    match od with
    | Digit(_, [||]) -> true
    | _ -> false

let isAllUnambiguousDigits digits = 
    digits
    |> Array.forall isUnambiguousDigit

let isIllegible od = 
    match od with
    | Ambiguous([||]) -> true
    | _ -> false

let getDigitVal od = 
    match od with
    | Digit(dv, _) -> dv
    | _ -> failwith "not a digit"

let getAlternates od = 
    match od with
    | Digit(_, alternates) -> alternates
    | Ambiguous(alternates) -> alternates

let isAmbiguous od = 
    match od with
    | Ambiguous(possibilities) when possibilities.Length > 0 -> true
    | _ -> false

let private zeroStrings = 
    (
        " _ ",
        "| |",
        "|_|"
    )

let private oneStrings = 
    (
        "   ",
        "  |",
        "  |"
    )

let private twoStrings = 
    (
        " _ ",
        " _|",
        "|_ "
    )

let private threeStrings = 
    (
        " _ ",
        " _|",
        " _|"
    )

let private fourStrings = 
    (
        "   ",
        "|_|",
        "  |"
    )

let private fiveStrings = 
    (
        " _ ",
        "|_ ",
        " _|"
    )

let private sixStrings = 
    (
        " _ ",
        "|_ ",
        "|_|"
    )

let private sevenStrings = 
    (
        " _ ",
        "  |",
        "  |"
    )

let private eightStrings = 
    (
        " _ ",
        "|_|",
        "|_|"
    )

let private nineStrings = 
    (
        " _ ",
        "|_|",
        " _|"
    )

let digitMap = 
    [|
        (zeroStrings, Digit (0, [||]))
        (oneStrings, Digit (1, [||]))
        (twoStrings, Digit (2, [||]))
        (threeStrings, Digit (3, [||]))
        (fourStrings, Digit (4, [||]))
        (fiveStrings, Digit (5, [||]))
        (sixStrings, Digit (6, [||]))
        (sevenStrings, Digit (7, [||]))
        (eightStrings, Digit (8, [||]))
        (nineStrings, Digit (9, [||]))
    |]
    |> Map.ofArray

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Definitions for processed output

type ProcessedOutput =  
    | ValidOutput of int array
    | AmbiguousOutput of OcrDigit array * OcrDigit array array
    | IllegibleOutput of OcrDigit array

let isValidAccountInt(digits: int array) = 
    match digits with
    | [| d9; d8; d7; d6; d5; d4; d3; d2; d1 |] ->
        let sum = d1 + 2 * d2 + 3 * d3 + 4 * d4 + 5 * d5 + 6 * d6 + 7 * d7 + 8 * d8 + 9 * d9
        sum % 11 = 0
    | _ -> false

let isValidAccount(digits: OcrDigit list) = 
    let ints = 
        digits
        |> Array.ofList
        |> Array.map(fun d -> 
                        match d with 
                        | Digit(e, [||]) -> e 
                        | _ -> failwith "only use with definite digits")
    isValidAccountInt(ints)

let rec digitGroupPossibilities(remaining: OcrDigit list): OcrDigit list seq = 
    seq {
        match remaining with
        | x :: tail -> 
            let rest = digitGroupPossibilities(tail)
            let toPrepend = 
                match x with
                | Digit(d, others) ->
                    others |> Array.append([| d |])
                | Ambiguous(others) -> 
                    others

            yield! rest |> Seq.collect(fun l -> toPrepend |> Array.map(fun newdigit -> Digit(newdigit, [||]) :: l))
        | [] -> ()
    }

let processDigitGroup(digits: OcrDigit array): ProcessedOutput = 
    let possibilities = digitGroupPossibilities(digits |> List.ofArray)

    match possibilities |> Seq.tryHead with
    | Some(first) when isAllUnambiguousDigits(first |> Array.ofList) && isValidAccount(first) ->
        // if there is an unambiguous reading with a correct checksum, prefer that.
        ValidOutput(first |> Array.ofList |> Array.map getDigitVal)
    | Some(_) -> 
        // Otherwise, generate all possibilities. If there is only
        // one that would generate a valid checksum, use that.
        // If there is more than one, report ambiguous output.
        // If none, report illegible output.
        let valid = 
            possibilities
            |> Seq.filter isValidAccount
            |> Array.ofSeq

        match valid with
        | [| onlyValid |] -> ValidOutput(onlyValid |> Array.ofList |> Array.map getDigitVal)
        | [||] -> IllegibleOutput(digits)
        | v -> AmbiguousOutput(digits, v |> Array.map Array.ofList)
        
    | _ -> IllegibleOutput(digits)

let ocrDigitToString(d: OcrDigit) = 
    match d with
    | Digit(d, [||]) -> d.ToString()
    | _ -> "?"

let outputProcessedGunk(o: ProcessedOutput) = 
    match o with
    | ValidOutput(ds) -> 
        ds |> Array.iter Console.Write
        Console.WriteLine()
    | AmbiguousOutput(original, possibilities) -> 
        Console.Write(String.Join("", original |> Array.map ocrDigitToString))
        Console.Write(" AMB [")
        let possibilityStrings = 
            String.Join(", ", possibilities |> Array.collect (fun ps -> ps |> Array.map ocrDigitToString))

        Console.Write(possibilityStrings)
        Console.WriteLine("]")
    | IllegibleOutput(original) ->
        Console.Write(String.Join("", original |> Array.map ocrDigitToString))
        Console.WriteLine(" ILL")
    
/////////////////////////////////////////////////////////////////////////////

let digitBoxesForChunk(lines: string array) = 
    match lines with
    | [| one; two; three |] -> 
        let oneSlices = one.ToCharArray() |> Array.chunkBySize 3 |> Array.filter(fun a -> a.Length = 3) |> Array.map(fun c -> new string(c))
        let twoSlices = two.ToCharArray() |> Array.chunkBySize 3 |> Array.filter(fun a -> a.Length = 3) |> Array.map(fun c -> new string(c))
        let threeSlices = three.ToCharArray() |> Array.chunkBySize 3 |> Array.filter(fun a -> a.Length = 3) |> Array.map(fun c -> new string(c))
        Array.zip3 oneSlices twoSlices threeSlices
    | _ -> failwith "only handles digits three digits high"

let digitBoxForGroup(group: string array) = 
    let digitBoxForTuple(db: string * string * string) = 
        let (one, two, three) = db

        let alternates = 
            [|
                (one.Remove(0, 1).Insert(0, "|"), two, three)
                (one.Remove(1, 1).Insert(1, "|"), two, three)
                (one.Remove(2, 1).Insert(2, "|"), two, three)
                (one, two.Remove(0, 1).Insert(0, "|"), three)
                (one, two.Remove(1, 1).Insert(1, "|"), three)
                (one, two.Remove(2, 1).Insert(2, "|"), three)
                (one, two, three.Remove(0, 1).Insert(0, "|"))
                (one, two, three.Remove(1, 1).Insert(1, "|"))
                (one, two, three.Remove(2, 1).Insert(2, "|"))

                (one.Remove(0, 1).Insert(0, "_"), two, three)
                (one.Remove(1, 1).Insert(1, "_"), two, three)
                (one.Remove(2, 1).Insert(2, "_"), two, three)
                (one, two.Remove(0, 1).Insert(0, "_"), three)
                (one, two.Remove(1, 1).Insert(1, "_"), three)
                (one, two.Remove(2, 1).Insert(2, "_"), three)
                (one, two, three.Remove(0, 1).Insert(0, "_"))
                (one, two, three.Remove(1, 1).Insert(1, "_"))
                (one, two, three.Remove(2, 1).Insert(2, "_"))
            |]

        let alternateDigits = 
            alternates
            |> Array.map(fun db -> digitMap |> Map.tryFind(db))
            |> Array.filter(fun digitOpt -> digitOpt.IsSome)
            |> Array.map(fun digitOpt -> digitOpt.Value)

        match digitMap |> Map.tryFind(db) with
        | Some(Digit(digitVal, _)) -> 
            Digit(digitVal, alternateDigits |> Array.map getDigitVal)
        | _ -> 
            Ambiguous(alternateDigits |> Array.map getDigitVal)

    digitBoxesForChunk(group) |> Array.map digitBoxForTuple

let readFile(fileName: string) = 
    let allText = File.ReadAllText(fileName)
    let allLines = 
        allText.Split([| Environment.NewLine |], StringSplitOptions.None)

    let lineGroups = 
        allLines
        |> Array.chunkBySize 3
        |> Array.filter(fun a -> a.Length = 3)

    lineGroups
    |> Array.map(digitBoxForGroup >> processDigitGroup)
    |> Array.iter outputProcessedGunk
    
readFile(@"sample")
