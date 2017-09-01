// BankOCR Kata (from http://codingdojo.org/kata/BankOCR/ )
// by Erik Novales (enovales@codehitmen.com)

open System
open System.IO

// sketch:
// 1) Create a discriminated union of OCR digits (valid/invalid)
// 2) For each line group, check to see if the digits that are read satisfy the
//    checksum.
// 3) If so, then output the digits.
// 4) If not, generate all single-error permutations.
// 5) If there is exactly one single-error permutation that is valid, then
//    output that.
// 6) If there is more than one single-error permutation that has a valid
//    checksum, then output the original digits, marked as ambiguous, and then
//    all valid possibilities.
// 7) If there are no valid single-error permutations, then output the original
//    digits, marked as illegible.

/////////////////////////////////////////////////////////////////////////////
// OcrBox discriminated union and utilities

type OcrBox = 
  | Digit of int
  | Invalid

let getDigits(boxes: OcrBox array) = 
    let digitOptForBox(box: OcrBox) = 
        match box with
        | Digit(d) -> Some(d)
        | _ -> None

    boxes
    |> Array.collect(digitOptForBox >> Option.toArray)

let digitsToString(boxes: OcrBox array) = 
    let charForDigit(box: OcrBox) = 
        match box with
        | Digit(d) -> d.ToString().Chars(0)
        | _ -> '?'

    new String(boxes |> Array.map charForDigit)

let private digitMap = 
    let zeroStrings = 
        (
            " _ ".ToCharArray(),
            "| |".ToCharArray(),
            "|_|".ToCharArray()
        )

    let oneStrings = 
        (
            "   ".ToCharArray(),
            "  |".ToCharArray(),
            "  |".ToCharArray()
        )

    let twoStrings = 
        (
            " _ ".ToCharArray(),
            " _|".ToCharArray(),
            "|_ ".ToCharArray()
        )

    let threeStrings = 
        (
            " _ ".ToCharArray(),
            " _|".ToCharArray(),
            " _|".ToCharArray()
        )

    let fourStrings = 
        (
            "   ".ToCharArray(),
            "|_|".ToCharArray(),
            "  |".ToCharArray()
        )

    let fiveStrings = 
        (
            " _ ".ToCharArray(),
            "|_ ".ToCharArray(),
            " _|".ToCharArray()
        )

    let sixStrings = 
        (
            " _ ".ToCharArray(),
            "|_ ".ToCharArray(),
            "|_|".ToCharArray()
        )

    let sevenStrings = 
        (
            " _ ".ToCharArray(),
            "  |".ToCharArray(),
            "  |".ToCharArray()
        )

    let eightStrings = 
        (
            " _ ".ToCharArray(),
            "|_|".ToCharArray(),
            "|_|".ToCharArray()
        )

    let nineStrings = 
        (
            " _ ".ToCharArray(),
            "|_|".ToCharArray(),
            " _|".ToCharArray()
        )

    [|
        (zeroStrings, Digit(0))
        (oneStrings, Digit(1))
        (twoStrings, Digit(2))
        (threeStrings, Digit(3))
        (fourStrings, Digit(4))
        (fiveStrings, Digit(5))
        (sixStrings, Digit(6))
        (sevenStrings, Digit(7))
        (eightStrings, Digit(8))
        (nineStrings, Digit(9))
    |]
    |> Map.ofArray

/////////////////////////////////////////////////////////////////////////////


// Returns a sequence of all single-error permutations for the input string.
let permuteString(s: string) = 
    let chars = s.ToCharArray()
    seq {
        for c in ['|'; '_'; ' '] do
            for i in 0..chars.Length - 1 do
                match chars.[i] with
                | '\r' | '\n' -> ()
                | _ -> 
                    let newVal = Array.copy(chars)
                    newVal.[i] <- c
                    yield new String(newVal)
    }

// Returns an array of OCR digits for a group of lines.
let digitsForLineGroup(lineGroup: string array): OcrBox array = 
    let digitForLineGroup(boxTuple: char array * char array * char array) = 
        match digitMap.TryFind(boxTuple) with
        | Some(d) -> d
        | _ -> Invalid

    match lineGroup with
    | [| lineOne; lineTwo; lineThree |] ->
        let lineOneChunks = lineOne.ToCharArray() |> Array.chunkBySize(3)
        let lineTwoChunks = lineTwo.ToCharArray() |> Array.chunkBySize(3)
        let lineThreeChunks = lineThree.ToCharArray() |> Array.chunkBySize(3)

        Array.zip3(lineOneChunks)(lineTwoChunks)(lineThreeChunks)
        |> Array.map digitForLineGroup
    | _ -> failwith "need to use 3-length line group"

// Returns whether the checksum for a given set of digits is valid.
let isValidAccountInt(digits: int array) = 
    match digits with
    | [| d9; d8; d7; d6; d5; d4; d3; d2; d1 |] ->
        let sum = d1 + 2 * d2 + 3 * d3 + 4 * d4 + 5 * d5 + 6 * d6 + 7 * d7 + 8 * d8 + 9 * d9
        sum % 11 = 0
    | _ -> false

// Returns whether a set of OCR digits represents a value with a valid checksum.
let isValidAccount(digits: OcrBox array) = 
    let digitInts = getDigits(digits)
    if (digitInts.Length <> 9) then
        false
    else
        isValidAccountInt(digitInts)

let splitLines(s: string) = 
    s.Split([| Environment.NewLine |], StringSplitOptions.None)

// Runs the kata. Takes the filename of the input file as a parameter.
let runKata(fileName: string) = 
    let lineGroups = 
        File.ReadAllLines(fileName)
        |> Array.chunkBySize(3)
        |> Array.filter(fun g -> g.Length = 3)
        |> Array.map(fun g -> String.Join(Environment.NewLine, g))

    let runKataForLineGroup(original: string) = 
        let originalDigitBoxes = digitsForLineGroup(splitLines(original))
        if isValidAccount(originalDigitBoxes) then
            System.Console.WriteLine(digitsToString(originalDigitBoxes))
        else
            let validPermutations = 
                permuteString(original)
                |> Seq.map (splitLines >> digitsForLineGroup)
                |> Seq.filter isValidAccount

            match validPermutations |> Seq.length with
            | 1 ->
                System.Console.WriteLine(digitsToString(validPermutations |> Seq.head))
            | 0 ->
                System.Console.Write(digitsToString(originalDigitBoxes))
                System.Console.WriteLine(" ILL")
            | _ -> 
                System.Console.Write(digitsToString(originalDigitBoxes))
                System.Console.Write(" AMB [")
                System.Console.Write(String.Join(", ", validPermutations |> Seq.map digitsToString))
                System.Console.WriteLine("]")

    lineGroups
    |> Array.iter runKataForLineGroup
