module App

open Browser.Dom
open Fable.Core.Util
open Fable.Core.JsInterop
open Fable.Core


// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement
let numberInput = document.getElementById("number") :?> Browser.Types.HTMLInputElement

//can't be in a method
let [<Global("Audio")>] audioType : Browser.Types.HTMLAudioElementType = jsNative


let setOnEnded (a1: Browser.Types.HTMLAudioElement) (a2: Browser.Types.HTMLAudioElement) =
    a1.onended <- fun _ ->
        a2.play()

let createAudioBuilder (path: string) (fileType: string) (name: string) = 
    let audio = audioType.Create()
    let clip = $"{path}/{name}.{fileType}"
    console.log(clip);
    audio.src <- clip
    audio

let identifyNumber (number: int) =
    match number with
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "forty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "seventy"
    | 80 -> "eighty"
    | 90 -> "ninty"
    | _ -> "zero"

let printList (names: string list) =
    for name in names do
        console.log name

let parseThreeDigit (number: string) =
    let mutable sounds = []
    let num = number |> int

    let hundreds = num / 100
    if hundreds > 0 
    then 
        //console.log("hundreds: " + hundreds.ToString())
        let hun = identifyNumber hundreds
        //console.log(hun + " hundred")
        sounds <- List.append sounds [hun; "hundred"]

    let remainder = num % 100

    if remainder > 19 
    then 
        let tens = (remainder / 10) * 10
        
        let tensString = if tens > 0 then Some(identifyNumber tens) else None
        match tensString with 
        | Some n -> 
            sounds <- List.append sounds [n]
        | None -> ()

        let ones = remainder % 10

        let onesString = if ones > 0 then Some(identifyNumber ones) else None
        match onesString with 
        | Some n -> 
            sounds <- List.append sounds [n]
        | None -> ()

    else if remainder > 0
    then
        let tens = identifyNumber remainder
        sounds <- List.append sounds [tens]
    
    printList sounds
    sounds
    
let parseNumber (number: string) =
    let numberName =
        number 
        |> int
        |> identifyNumber
    console.log("NumberName: " + numberName)

    numberName

let buildAudio (createAudio: string -> Browser.Types.HTMLAudioElement) (name: string) =    
    createAudio name

let bindAudios (audios: Browser.Types.HTMLAudioElement list) =
    match audios.Length with
    | 1 -> audios
    | _ -> 
        let mutable count = 0
        let mutable previousAudio = audios.Head
        for audio in audios do
            if count <> 0 
            then 
                setOnEnded previousAudio audio
                previousAudio <- audio
            count <- count + 1
        audios

let processNumbers () =
    let createAudio = createAudioBuilder "./audio" "mp3"

    let number = numberInput.value

    let boundAudios =
        parseThreeDigit number
        |> List.map (buildAudio createAudio)
        |> bindAudios

    console.log ("AudioCount: " + boundAudios.Length.ToString())
    boundAudios.Head.play()


myButton.onclick <- fun _ ->
    processNumbers()
    
    0

numberInput.addEventListener("keyup", fun (event: Browser.Types.Event) -> 
    //regular event doesn't have `key`, so a cast is required
    let keyEvent = event :?> Browser.Types.KeyboardEvent;

    //match against a particular key
    if keyEvent.key = "Enter" 
    then processNumbers()
    else if keyEvent.key = "Shift"
    then numberInput.value <- ""

)
