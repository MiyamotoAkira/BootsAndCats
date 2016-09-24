





#r "node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser



// First a moving angle
// Then a power
// calculate where boot will fall
// far from cat nothing happens
// near cat, cat moves a fixed amount opposite direction
// need to get cat out of the garden


module Keyboard =
    let mutable keysPressed = Set.empty

    let code x = if keysPressed.Contains(x) then 1 else 0

    let update (e: KeyboardEvent, pressed) =
        let keyCode = int e.keyCode
        let op = if pressed then Set.add else Set.remove
        keysPressed <- op keyCode keysPressed
        null

    let init() =
        document.addEventListener_keydown(fun e -> update (e, true))
        document.addEventListener_keyup(fun e-> update(e, false))
