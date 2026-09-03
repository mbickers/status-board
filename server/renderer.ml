open! Core
open! Async

module Device_status = struct
  type t = { battery_voltage : float option } [@@deriving sexp]
end

module Input = struct
  type 'debug_preset t =
    | Device of Device_status.t
    | Preview of 'debug_preset option
end

module Render = struct
  type t =
    { buffer : Image.image
    ; time_until_refresh : Time_ns.Span.t
    }
end

type 'debug_preset t =
  { debug_presets : 'debug_preset list
  ; debug_preset_name : 'debug_preset -> string
  ; render : 'debug_preset Input.t -> Cache.t -> Render.t Deferred.Or_error.t
  }

type packed = Pack : 'debug_preset t -> packed

let debug_preset_names (Pack t) = List.map t.debug_presets ~f:t.debug_preset_name

let render_device (Pack t) device_status cache =
  t.render (Input.Device device_status) cache
;;

let render_preview (Pack t) ~debug_preset cache =
  let debug_preset =
    match debug_preset with
    | None | Some "" -> Ok None
    | Some name ->
      List.find t.debug_presets ~f:(fun preset ->
        String.equal name (t.debug_preset_name preset))
      |> Option.value_map
           ~default:(Or_error.errorf "Unknown debug preset %S" name)
           ~f:(fun preset -> Ok (Some preset))
  in
  match debug_preset with
  | Ok debug_preset -> t.render (Input.Preview debug_preset) cache
  | Error error -> return (Error error)
;;
