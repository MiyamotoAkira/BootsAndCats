#r "node_modules/fable-core/Fable.Core.dll"
open System
open Fable.Core
open Fable.Import.Browser

// angle has to change based on input (angle)
// force has to change based on input (vertical slide)

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
   
    
let g = 9.8

let calculatevy force angle  =
    let angle' = angle * (Math.PI/180.)
    (force *  (sin angle'))

type Boot =
    {
      x: float
      y: float
      vy: float
      img: string
    }

let bootIsInGround boot =
    boot.y >= 0. && boot.vy <= 0.

let calculateBootPosition boot (originalForce: float)  (originalAngle :float) (time:float) =
    if bootIsInGround boot
    then
        { boot with y = 0.}
    else
        let angle = originalAngle * (Math.PI/180.)
        { boot with
            x = originalForce * (time/1000.) * (cos angle)
            y = -1. * (originalForce * (time/1000.) * (sin angle) - (1./2.) * g * ((time/1000.)**2.))
            vy = (originalForce * (sin angle) - g * (time/1000.))
        }

type Cat =
    { x: float;
      img: string}

type Gamestate =
    | Initial
    | NewBoot of Cat
    | InProgress of (Cat * Boot * int * float * float * float)
    | Finished

let moveCat (cat : Cat) (boot: Boot) =
    if boot.x < cat.x then
        { cat with x = cat.x + 60. }
    else
        { cat with x = cat.x - 100. }

let calculateCatPosition cat boot time =
    if bootIsInGround boot then
        if abs (cat.x - boot.x) < 10. then
          moveCat cat boot
        else
          cat
    else
        cat

let calculateNewGameState boot numberOfBoots originalForce originalAngle time (cat:Cat) =
    if numberOfBoots = 0 then Finished
    else if cat.x > 400. then Finished
    else if bootIsInGround boot then
        InProgress (cat, boot, numberOfBoots - 1, originalForce, originalAngle, time)
        else
            InProgress (cat, boot, numberOfBoots, originalForce, originalAngle, time)
    

let render (w,h) cat (boot:Boot) =
    (0.,0.,w,h) |> Win.filled (Win.rgb 174 238 238)
    (0., h-50., w, 50.) |> Win.filled (Win.rgb 74 163 41)

    cat.img |> Win.image "cat" |> Win.position (cat.x, h-50.-16.)
    boot.img |> Win.image "boot" |> Win.position (boot.x, h-50.-16. + boot.y)
    

let w, h = Win.dimensions()

type Entry =
    {
        angle : float
        force : float
        dirty : bool
    }

let mutable entry = {angle =  0.; force =  0.; dirty = false}

// document.getElementById "throwvalues" ? onsubmit (fun event ->
//                                                   event?preventDefault()
//                                                   let angleValue = document.getElementById "angle" ? value
//                                                   let forceValue = document.getElementById "force" ? value
//                                                   let angle = float angleValue
//                                                   let force = float forceValue

//                                                   entry <- {
//                                                        angle = angle
//                                                        force = force
//                                                        dirty = true
//                                                   })
                                                            

let rec gameloop gamestate () =
    match gamestate with
        | Finished -> gamestate
        | NewBoot cat ->
            let angle = 45.
            let force = 50.5
            let boot = {x = 0.;  y = 0.; vy = calculatevy force angle; img="boot.png"}

            if entry.dirty then
                render (w,h) cat boot            
                window.setTimeout(gameloop (InProgress (cat, boot, 5, entry.force, entry.angle, 0.)), 1000. / 6.) |> ignore
                entry <- { angle = angle; force = force; dirty = false}
            else
                window.setTimeout(gameloop (NewBoot cat), 1000. / 6.) |> ignore
            gamestate
        | Initial ->
            let cat = { x= 260.; img = "cat.png"}
            let angle = 45.
            let force = 50.5
            let boot = {x = 0.;  y = 0.; vy = calculatevy force angle; img="boot.png"}
            //let boot = {x = 0.;  y = 0.; vy = 0.; img="boot.png"}
            render (w,h) cat boot
            window.setTimeout(gameloop (InProgress (cat, boot, 5, force, angle, 0.)), 1000. / 6.) |> ignore
            //window.setTimeout(gameloop (NewBoot cat), 1000. / 6.) |> ignore
            gamestate
        | InProgress (cat, boot, numberOfBoots, originalForce, originalAngle, time) ->
            let boot' = calculateBootPosition boot  originalForce  originalAngle  time
            let cat'=  calculateCatPosition cat boot' time
            let gamestate' = calculateNewGameState boot' numberOfBoots originalForce originalAngle (time + 1000./6.)  cat'
            render (w,h) cat' boot'
            window.setTimeout(gameloop gamestate', 1000. / 6.) |> ignore
            gamestate'

gameloop Initial ()
