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

module Win =
    let canvas = document.getElementsByTagName_canvas().[0]
    let context = canvas.getContext_2d()

    let ($) s n = s + n.ToString()
    let rgb  r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

    let filled color rect =
        let ctx = context
        ctx.fillStyle <- U3.Case1 color
        ctx.fillRect rect

    let position (x,y) (img: HTMLImageElement) =
        img.style.left <- x.ToString() + "px"
        img.style.top <- (canvas.offsetTop + y).ToString() + "px"

    let dimensions () =
        canvas.width, canvas.height

    let image (src:string) =
        let image = document.getElementsByTagName_img().[0]
        if image.src.IndexOf(src) = -1 then image.src <- src
        image
   
    
let calculatevx force angle  = force *  (cos angle)
let calculatevy force angle  = force *  (sin angle)

let g = 9.8

type Wellie =
    { x: float;
      y: float;
      vx: float;
      vy: float;
    }

let calculateWelliePosition wellie (originalForce: float)  (originalAngle :float) (time:float) =
    if wellie.y = 0. && wellie.vy <= 0.
    then
        { wellie with vy = 0.; vx = 0. }
    else
        {
            x = originalForce * time * (cos originalAngle);
            y = originalForce * time * (sin originalAngle) - (1./2.) * g * (time**2.);
            vx = originalForce * time * (cos originalAngle)
            vy = originalForce  * (sin originalAngle) - g * time
        }

type Cat =
    { x: float;
      img: string}

type Gamestate =
    | Initial
    | InProgress of (Cat * Wellie * int * float * float * float)
    | Finished

let moveCat cat wellie =
    cat

let calculateCatPosition cat wellie =
    if wellie.y = 0. && wellie.vy = 0. then
        if abs (cat.x - wellie.x) < 0. then
          moveCat cat wellie
        else
          cat
    else
        cat

let calculateNewGameState wellie numberOfWellies originalForce originalAngle time (cat:Cat) =
    if numberOfWellies = 0 then Finished
    else if cat.x > 100. then Finished
    else InProgress (cat, wellie, numberOfWellies, originalForce, originalAngle, time)
    

let rec gameloop gamestate =
    match gamestate with
        | Finished -> gamestate
        | Initial ->
            let  cat = { x= 50.; img = ""}
            let angle = 45.
            let force = 50.
            let vx' = 0.;
            let vy' = 0.;
            gameloop (InProgress (cat, {x = 0.; y = 0.; vx = vx'; vy = vy'}, 5, force, angle, 0.))
        | InProgress (cat, wellie, numberOfWellies, originalForce, originalAngle, time) ->
            let wellie' =calculateWelliePosition wellie  originalForce  originalAngle  time
            let gamestate' =
                calculateCatPosition cat wellie'
                |> calculateNewGameState wellie' numberOfWellies originalForce originalAngle time
            gameloop gamestate'
