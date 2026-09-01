open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Forecast_hourly = struct
  type t =
    { time : string list
    ; precipitation_probability : int option list
    ; weather_code : int option list
    ; uv_index : float option list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Forecast_daily = struct
  type t = { sunset : string option list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Forecast = struct
  type t =
    { timezone : string
    ; hourly : Forecast_hourly.t
    ; daily : Forecast_daily.t
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Air_quality_current = struct
  type t = { us_aqi : float option } [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Air_quality = struct
  type t = { current : Air_quality_current.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

let fetch ~url ~decoder =
  let%bind.Deferred.Or_error contents = Http.get_body url in
  let%bind.Deferred.Or_error json =
    Or_error.try_with (fun () -> Yojson.Safe.from_string contents) |> return
  in
  Or_error.try_with (fun () -> decoder json) |> return
;;

module Coordinates = struct
  type t =
    { latitude : float
    ; longitude : float
    }
end

module Conditions = struct
  type t =
    { thunderstorm : bool
    ; cloudy : bool
    ; snow : bool
    }
end

(* Open-Meteo documents the WMO weather codes at
   https://open-meteo.com/en/docs#weathervariables. *)
let classify_weather_code weather_code =
  { Conditions.thunderstorm = List.mem [ 95; 96; 99 ] weather_code ~equal:Int.equal
  ; cloudy = weather_code = 2 || weather_code = 3
  ; snow = List.mem [ 71; 73; 75; 77; 85; 86 ] weather_code ~equal:Int.equal
  }
;;

module Summary = struct
  type t =
    { max_uv : float option
    ; sunset : Time_ns.Alternate_sexp.t option
    ; precipitation_probabilities : (Time_ns.Alternate_sexp.t * int) list
    ; thunderstorm : bool
    ; cloudy : bool
    ; snow : bool
    ; us_aqi : float option
    }
  [@@deriving sexp]
end

type hour =
  { time : Time_ns.t
  ; precipitation_probability : int option
  ; weather_code : int option
  ; uv_index : float option
  }

let parse_time ~zone time =
  Or_error.try_with (fun () -> Time_ns.of_localized_string ~zone time)
;;

let rec combine_hours ~zone ~times ~precipitation_probabilities ~weather_codes ~uv_indices
  =
  match times, precipitation_probabilities, weather_codes, uv_indices with
  | [], [], [], [] -> Ok []
  | ( time :: times
    , precipitation_probability :: precipitation_probabilities
    , weather_code :: weather_codes
    , uv_index :: uv_indices ) ->
    let%bind.Or_error time = parse_time ~zone time in
    let%map.Or_error hours =
      combine_hours ~zone ~times ~precipitation_probabilities ~weather_codes ~uv_indices
    in
    { time; precipitation_probability; weather_code; uv_index } :: hours
  | _ -> Or_error.error_string "Open-Meteo returned hourly arrays of different lengths"
;;

let summarize ~now ~forecast ~air_quality =
  let%bind.Or_error zone =
    Or_error.try_with (fun () -> Time_ns_unix.Zone.find_exn forecast.Forecast.timezone)
  in
  let%bind.Or_error hours =
    combine_hours
      ~zone
      ~times:forecast.hourly.time
      ~precipitation_probabilities:forecast.hourly.precipitation_probability
      ~weather_codes:forecast.hourly.weather_code
      ~uv_indices:forecast.hourly.uv_index
  in
  let hours = List.filter hours ~f:(fun hour -> Time_ns.compare hour.time now >= 0) in
  let%map.Or_error sunset =
    match List.hd forecast.daily.sunset |> Option.join with
    | None -> Ok None
    | Some sunset -> parse_time ~zone sunset |> Or_error.map ~f:Option.some
  in
  let conditions =
    hours
    |> List.filter_map ~f:(fun hour -> hour.weather_code)
    |> List.map ~f:classify_weather_code
  in
  { Summary.max_uv =
      hours
      |> List.filter_map ~f:(fun hour -> hour.uv_index)
      |> List.max_elt ~compare:Float.compare
  ; sunset
  ; precipitation_probabilities =
      List.filter_map hours ~f:(fun hour ->
        Option.map hour.precipitation_probability ~f:(fun percent -> hour.time, percent))
  ; thunderstorm = List.exists conditions ~f:(fun conditions -> conditions.thunderstorm)
  ; cloudy = List.exists conditions ~f:(fun conditions -> conditions.cloudy)
  ; snow = List.exists conditions ~f:(fun conditions -> conditions.snow)
  ; us_aqi = air_quality.Air_quality.current.us_aqi
  }
;;

let fetch_summary ~coordinates () =
  let latitude = coordinates.Coordinates.latitude
  and longitude = coordinates.longitude in
  let%bind.Deferred.Or_error forecast, air_quality =
    Deferred.Or_error.both
      (fetch
         ~url:
           [%string
             "https://api.open-meteo.com/v1/forecast?latitude=%{latitude#Float}&longitude=%{longitude#Float}&hourly=precipitation_probability,weather_code,uv_index&daily=sunset&timezone=auto&forecast_days=1"]
         ~decoder:Forecast.t_of_yojson)
      (fetch
         ~url:
           [%string
             "https://air-quality-api.open-meteo.com/v1/air-quality?latitude=%{latitude#Float}&longitude=%{longitude#Float}&current=us_aqi&timezone=auto"]
         ~decoder:Air_quality.t_of_yojson)
  in
  return (summarize ~now:(Time_ns.now ()) ~forecast ~air_quality)
;;

let query cache ~coordinates =
  Cache.get
    cache
    (module Summary)
    ~max_age:(Time_ns.Span.of_min 15.)
    ~fetch:(fetch_summary ~coordinates)
    ~key:
      [%string
        "weather_%{coordinates.Coordinates.latitude#Float}_%{coordinates.longitude#Float}"]
;;
