open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Forecast_hourly = struct
  type t =
    { time : string list
    ; temperature_2m : float option list
    ; precipitation_probability : int option list
    ; weather_code : int option list
    ; uv_index : float option list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Forecast_current = struct
  type t =
    { temperature_2m : float option
    ; uv_index : float option
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Forecast_daily = struct
  type t =
    { sunrise : string option list
    ; sunset : string option list
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Forecast = struct
  type t =
    { timezone : string
    ; current : Forecast_current.t
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
    { zone : Time_ns_unix.Zone.t
    ; current_temperature_celsius : float option
    ; low_temperature_celsius : float option
    ; high_temperature_celsius : float option
    ; current_uv_index : float option
    ; uv_indices : (Time_ns.Alternate_sexp.t * float) list
    ; sunrise : Time_ns.Alternate_sexp.t
    ; sunset : Time_ns.Alternate_sexp.t
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
  ; temperature_celsius : float option
  ; precipitation_probability : int option
  ; weather_code : int option
  ; uv_index : float option
  }

let parse_time ~zone time =
  Or_error.try_with (fun () ->
    Time_ns.of_localized_string ~zone (String.tr time ~target:'T' ~replacement:' '))
;;

let parse_daily_time ~zone ~name times =
  match List.hd times |> Option.join with
  | None -> Or_error.errorf "Open-Meteo returned no %s" name
  | Some time -> parse_time ~zone time
;;

let rec combine_hours
          ~zone
          ~times
          ~temperatures
          ~precipitation_probabilities
          ~weather_codes
          ~uv_indices
  =
  match times, temperatures, precipitation_probabilities, weather_codes, uv_indices with
  | [], [], [], [], [] -> Ok []
  | ( time :: times
    , temperature_celsius :: temperatures
    , precipitation_probability :: precipitation_probabilities
    , weather_code :: weather_codes
    , uv_index :: uv_indices ) ->
    let%bind.Or_error time = parse_time ~zone time in
    let%map.Or_error hours =
      combine_hours
        ~zone
        ~times
        ~temperatures
        ~precipitation_probabilities
        ~weather_codes
        ~uv_indices
    in
    { time; temperature_celsius; precipitation_probability; weather_code; uv_index }
    :: hours
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
      ~temperatures:forecast.hourly.temperature_2m
      ~precipitation_probabilities:forecast.hourly.precipitation_probability
      ~weather_codes:forecast.hourly.weather_code
      ~uv_indices:forecast.hourly.uv_index
  in
  let forecast_ends_at = Time_ns.add now (Time_ns.Span.of_hr 24.) in
  let hours =
    List.filter hours ~f:(fun hour ->
      Time_ns.compare hour.time now >= 0
      && Time_ns.compare hour.time forecast_ends_at <= 0)
  in
  let%map.Or_error sunrise = parse_daily_time ~zone ~name:"sunrise" forecast.daily.sunrise
  and sunset = parse_daily_time ~zone ~name:"sunset" forecast.daily.sunset in
  let conditions =
    hours
    |> List.filter_map ~f:(fun hour -> hour.weather_code)
    |> List.map ~f:classify_weather_code
  in
  let temperatures =
    Option.to_list forecast.current.temperature_2m
    @ List.filter_map hours ~f:(fun hour -> hour.temperature_celsius)
  in
  { Summary.zone
  ; current_temperature_celsius = forecast.current.temperature_2m
  ; low_temperature_celsius = List.min_elt temperatures ~compare:Float.compare
  ; high_temperature_celsius = List.max_elt temperatures ~compare:Float.compare
  ; current_uv_index = forecast.current.uv_index
  ; uv_indices =
      List.filter_map hours ~f:(fun hour ->
        Option.map hour.uv_index ~f:(fun uv_index -> hour.time, uv_index))
  ; sunrise
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
             "https://api.open-meteo.com/v1/forecast?latitude=%{latitude#Float}&longitude=%{longitude#Float}&current=temperature_2m,uv_index&hourly=temperature_2m,precipitation_probability,weather_code,uv_index&daily=sunrise,sunset&timezone=auto&forecast_hours=25"]
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
