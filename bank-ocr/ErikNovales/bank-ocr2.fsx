// BankOCR Kata (from http://codingdojo.org/kata/BankOCR/ )
// by Erik Novales (enovales@codehitmen.com)

open System
open System.IO

// This program actually solves a slightly different problem than the one
// specified in the BankOCR kata. Instead of only allowing a single OCR
// mistake in a digit group, this one solves for an OCR mistake in each
// digit.
//
// Sketch:
// 1) Define a discriminated union for the OCR digits. The discriminated union
//    values can contain an exact match (for Digit) as well as alternate
//    possibilities, taking into account a single OCR mistake per digit.
// 2) Read in the input, split into line groups, and generate digits.
// 3) Check to see if the digits that are read satisfy the checksum.
// 4) If so, then output the digits.
// 5) If not, then generate all possibilities of digit sequences (based on
//    single-character errors in each digit).
// 6) If there is exactly one permutation that is valid, then output that.
// 7) If there is more than one permutation that has a valid checksum, then 
//    output the original digits, marked as ambiguous, and then all valid 
//    possibilities.
// 8) If there are no valid permutations, then output the original digits, 
//    marked as illegible.
/////////////////////////////////////////////////////////////////////////////
type OcrDigit = 
    | Digit of int * int array
    | Ambiguous of int array
    with
        override this.ToString(): string = 
            match this with
            | Digit(d, alternates) ->
                d.ToString() + " [" + String.Join(",", alternates) + "]"
            | Ambiguous(alternates) -> 
                "[" + String.Join(",", alternates) + "]"

let private isAmbiguous od = 
    match od with
    | Ambiguous(_) -> true
    | _ -> false

let private getDigitVal od = 
    match od with
    | Digit(dv, _) -> dv
    | _ -> failwith "not a digit"

let private digitMap = 
    let zeroStrings = 
        (
            " _ ",
            "| |",
            "|_|"
        )

    let oneStrings = 
        (
            "   ",
            "  |",
            "  |"
        )

    let twoStrings = 
        (
            " _ ",
            " _|",
            "|_ "
        )

    let threeStrings = 
        (
            " _ ",
            " _|",
            " _|"
        )

    let fourStrings = 
        (
            "   ",
            "|_|",
            "  |"
        )

    let fiveStrings = 
        (
            " _ ",
            "|_ ",
            " _|"
        )

    let sixStrings = 
        (
            " _ ",
            "|_ ",
            "|_|"
        )

    let sevenStrings = 
        (
            " _ ",
            "  |",
            "  |"
        )

    let eightStrings = 
        (
            " _ ",
            "|_|",
            "|_|"
        )

    let nineStrings = 
        (
            " _ ",
            "|_|",
            " _|"
        )

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
    if (digits |> List.exists isAmbiguous) then
        false
    else
        let ints = 
            digits
            |> Array.ofList
            |> Array.map(fun d -> 
                            match d with 
                            | Digit(e, _) -> e 
                            | _ -> failwith "only use with definite digits")
        isValidAccountInt(ints)

let rec digitGroupPossibilities(remaining: OcrDigit list): OcrDigit list seq = 
    seq {
        match remaining with
        | [x] ->
            match x with
            | Digit(d, others) ->
                yield [Digit(d, [||])]
                yield! others |> Array.map(fun newdigit -> [Digit(newdigit, [||])])
            | Ambiguous(others) ->
                yield! others |> Array.map(fun newdigit -> [Digit(newdigit, [||])])
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
    let possibilities = 
        digitGroupPossibilities(digits |> List.ofArray)

    match possibilities |> Seq.tryHead with
    | Some(first) when isValidAccount(first) ->
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

        match valid |> Seq.tryHead with
        | Some(onlyValid) when valid |> Seq.tail |> Seq.isEmpty -> ValidOutput(onlyValid |> Array.ofList |> Array.map getDigitVal)
        | Some(_) -> AmbiguousOutput(digits, valid |> Array.ofSeq |> Array.map Array.ofList)
        | None -> IllegibleOutput(digits)
        
    | _ -> IllegibleOutput(digits)

let ocrDigitToString(printDigitForAmbiguous: bool)(d: OcrDigit) = 
    match d with
    | Digit(d, _) when printDigitForAmbiguous -> d.ToString()
    | Digit(d, [||]) -> d.ToString()
    | _ -> "?"

let outputProcessedGunk(o: ProcessedOutput) = 
    match o with
    | ValidOutput(ds) -> 
        ds |> Array.iter Console.Write
        Console.WriteLine()
    | AmbiguousOutput(original, possibilities) -> 
        Console.Write(String.Join("", original |> Array.map(ocrDigitToString(true))))
        Console.Write(" AMB [")
        let possibilityStrings = 
            String.Join(", ", possibilities |> Array.map (fun ps -> String.Join("", ps |> Array.map(ocrDigitToString(false)))))

        Console.Write(possibilityStrings)
        Console.WriteLine("]")
    | IllegibleOutput(original) ->
        Console.Write(String.Join("", original |> Array.map(ocrDigitToString(false))))
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
                // add pipe
                (one.Remove(0, 1).Insert(0, "|"), two, three)
                (one.Remove(1, 1).Insert(1, "|"), two, three)
                (one.Remove(2, 1).Insert(2, "|"), two, three)
                (one, two.Remove(0, 1).Insert(0, "|"), three)
                (one, two.Remove(1, 1).Insert(1, "|"), three)
                (one, two.Remove(2, 1).Insert(2, "|"), three)
                (one, two, three.Remove(0, 1).Insert(0, "|"))
                (one, two, three.Remove(1, 1).Insert(1, "|"))
                (one, two, three.Remove(2, 1).Insert(2, "|"))

                // add underscore
                (one.Remove(0, 1).Insert(0, "_"), two, three)
                (one.Remove(1, 1).Insert(1, "_"), two, three)
                (one.Remove(2, 1).Insert(2, "_"), two, three)
                (one, two.Remove(0, 1).Insert(0, "_"), three)
                (one, two.Remove(1, 1).Insert(1, "_"), three)
                (one, two.Remove(2, 1).Insert(2, "_"), three)
                (one, two, three.Remove(0, 1).Insert(0, "_"))
                (one, two, three.Remove(1, 1).Insert(1, "_"))
                (one, two, three.Remove(2, 1).Insert(2, "_"))

                // remove contents
                (one.Remove(0, 1).Insert(0, " "), two, three)
                (one.Remove(1, 1).Insert(1, " "), two, three)
                (one.Remove(2, 1).Insert(2, " "), two, three)
                (one, two.Remove(0, 1).Insert(0, " "), three)
                (one, two.Remove(1, 1).Insert(1, " "), three)
                (one, two.Remove(2, 1).Insert(2, " "), three)
                (one, two, three.Remove(0, 1).Insert(0, " "))
                (one, two, three.Remove(1, 1).Insert(1, " "))
                (one, two, three.Remove(2, 1).Insert(2, " "))
            |]

        let alternateDigits = 
            alternates
            |> Array.map(fun db -> digitMap |> Map.tryFind(db))
            |> Array.filter(fun digitOpt -> digitOpt.IsSome)
            |> Array.map(fun digitOpt -> digitOpt.Value)
            |> Array.map getDigitVal

        match digitMap |> Map.tryFind(db) with
        | Some(Digit(digitVal, _)) -> 
            Digit(digitVal, alternateDigits |> Array.filter(fun d -> d <> digitVal))
        | _ -> Ambiguous(alternateDigits)

    digitBoxesForChunk(group) 
    |> Array.map digitBoxForTuple

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
    
let runKata(fileName: string) = 
    readFile(fileName)
