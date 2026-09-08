open! Core

module Horizontal_alignment = struct
  type t =
    | Left
    | Center
    | Right
end

module Vertical_alignment = struct
  type t =
    | Top
    | Center
    | Bottom
    | Baseline
end

module Anchor = struct
  type t = (Horizontal_alignment.t * int) * (Vertical_alignment.t * int)
end

type t =
  { size : int * int
  ; baseline : int
  ; paint : Context.t -> upper_left:int * int -> unit
  }

let size t = t.size

let create ?baseline ~size ~draw () =
  { size; baseline = Option.value baseline ~default:(snd size); paint = draw }
;;

let draw t context ((horizontal, x), (vertical, y)) =
  let width, height = t.size in
  let left =
    match horizontal with
    | Horizontal_alignment.Left -> x
    | Center -> x - (width / 2)
    | Right -> x - width
  and top =
    match vertical with
    | Vertical_alignment.Top -> y
    | Center -> y - (height / 2)
    | Bottom -> y - height
    | Baseline -> y - t.baseline
  in
  t.paint context ~upper_left:(left, top)
;;

let column ~gap ~align children =
  let width, height =
    List.fold children ~init:(0, 0) ~f:(fun (width, height) child ->
      let w, h = size child in
      Int.max width w, height + h + gap)
  in
  let height =
    match children with
    | [] -> 0
    | _ -> height - gap
  in
  create
    ~size:(width, height)
    ~draw:(fun context ~upper_left:(left, top) ->
      let x =
        match align with
        | Horizontal_alignment.Left -> left
        | Center -> left + (width / 2)
        | Right -> left + width
      in
      ignore
        (List.fold children ~init:top ~f:(fun top child ->
           draw child context ((align, x), (Top, top));
           top + snd (size child) + gap)
         : int))
    ()
;;
