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

    let image elementId (src:string) =
        let image = document.getElementById elementId :?> HTMLImageElement
        if image.src.IndexOf(src) = -1 then image.src <- src
        image
   
    
let calculatevx force angle  = force *  (cos angle)
let calculatevy force angle  = force *  (sin angle)

let g = 9.8

type Wellie =
    {
      x: float
      y: float
      vx: float
      vy: float
      img: string
    }

let calculateWelliePosition wellie (originalForce: float)  (originalAngle :float) (time:float) =
    if wellie.y = 0. && wellie.vy <= 0.
    then
        { wellie with vy = 0.; vx = 0. }
    else
        { wellie with
            x = originalForce * time * (cos originalAngle)
            y = originalForce * time * (sin originalAngle) - (1./2.) * g * (time**2.)
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
    else InProgress (cat, wellie, numberOfWellies - 1, originalForce, originalAngle, time)
    

let render (w,h) cat (wellie:Wellie) =
    (0.,0.,w,h) |> Win.filled (Win.rgb 174 238 238)
    (0., h-50., w, 50.) |> Win.filled (Win.rgb 74 163 41)

    cat.img |> Win.image "cat" |> Win.position (w/2. - 16. + cat.x, h-50.-16.)
    wellie.img |> Win.image "boot" |> Win.position (w/2. - 16. + wellie.x, h-50.-16. + wellie.y)
    

let w, h = Win.dimensions()

let rec gameloop gamestate () =
    match gamestate with
        | Finished -> gamestate
        | Initial ->
            let  cat = { x= 50.; img = "cat.png"}
            let angle = 45.
            let force = 50.
            let vx' = 0.;
            let vy' = 0.;
            let wellie = {x = 0.; y = 0.; vx = vx'; vy = vy'; img="boot.png"}
            render (w,h) cat wellie
            window.setTimeout(gameloop (InProgress (cat, wellie, 5, force, angle, 0.)), 1000. / 60.) |> ignore
            gamestate
        | InProgress (cat, wellie, numberOfWellies, originalForce, originalAngle, time) ->
            let wellie' = calculateWelliePosition wellie  originalForce  originalAngle  time
            let cat'=  calculateCatPosition cat wellie'
            let gamestate' = calculateNewGameState wellie' numberOfWellies originalForce originalAngle time cat'
            render (w,h) cat' wellie'
            window.setTimeout(gameloop gamestate', 1000. / 60.) |> ignore
            gamestate'

gameloop Initial
