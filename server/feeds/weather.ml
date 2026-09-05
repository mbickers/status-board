open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
    ; rain : bool
    ; snow : bool
    }
  [@@deriving sexp]
end

(* Open-Meteo documents the WMO weather codes at
   https://open-meteo.com/en/docs#weathervariables. *)
let classify_weather_code weather_code =
  { Conditions.thunderstorm = List.mem [ 95; 96; 99 ] weather_code ~equal:Int.equal
  ; cloudy = weather_code = 2 || weather_code = 3
  ; rain =
      List.mem
        [ 51; 53; 55; 56; 57; 61; 63; 65; 66; 67; 80; 81; 82 ]
        weather_code
        ~equal:Int.equal
  ; snow = List.mem [ 71; 73; 75; 77; 85; 86 ] weather_code ~equal:Int.equal
  }
;;

module Forecast_current = struct
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; interval_seconds : int
    ; temperature_2m : float option
    ; conditions : Conditions.t option
    ; uv_index : float option
    }
  [@@deriving sexp]
end

module Hourly = struct
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; temperature_2m : float option
    ; precipitation_probability : int option
    ; conditions : Conditions.t option
    ; uv_index : float option
    }
  [@@deriving sexp]
end

module Daily = struct
  type t =
    { date : Date.t
    ; sunrise : Time_ns.Alternate_sexp.t option
    ; sunset : Time_ns.Alternate_sexp.t option
    ; moon_phase : float option
    }
  [@@deriving sexp]
end

module Forecast = struct
  type t =
    { timezone : string
    ; current : Forecast_current.t
    ; hourly : Hourly.t list
    ; daily : Daily.t list
    }
  [@@deriving sexp]
end

module Air_quality_current = struct
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; interval_seconds : int
    ; us_aqi : float option
    }
  [@@deriving sexp]
end

module Air_quality = struct
  type t = { current : Air_quality_current.t } [@@deriving sexp]
end

module Raw = struct
  module Forecast_current = struct
    type t =
      { time : string
      ; interval : int
      ; temperature_2m : float option
      ; weather_code : int option
      ; uv_index : float option
      }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

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

  module Forecast_daily = struct
    type t =
      { time : string list
      ; sunrise : string option list
      ; sunset : string option list
      ; moon_phase : float option list
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
    type t =
      { time : string
      ; interval : int
      ; us_aqi : float option
      }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Air_quality = struct
    type t =
      { timezone : string
      ; current : Air_quality_current.t
      }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

let fetch ~url ~decoder =
  let%bind.Deferred.Or_error contents = Http.get_body url in
  let%bind.Deferred.Or_error json =
    Or_error.try_with (fun () -> Yojson.Safe.from_string contents) |> return
  in
  Or_error.try_with (fun () -> decoder json) |> return
;;

let parse_time ~zone time =
  Or_error.try_with (fun () ->
    Time_ns.of_localized_string ~zone (String.tr time ~target:'T' ~replacement:' '))
;;

let parse_times ~zone times =
  List.map times ~f:(parse_time ~zone) |> Or_error.combine_errors
;;

let parse_optional_times ~zone times =
  List.map
    times
    ~f:
      (Option.value_map ~default:(Ok None) ~f:(fun time ->
         Or_error.map (parse_time ~zone time) ~f:Option.some))
  |> Or_error.combine_errors
;;

let parse_forecast (forecast : Raw.Forecast.t) =
  let%bind.Or_error zone =
    Or_error.try_with (fun () -> Time_ns_unix.Zone.find_exn forecast.timezone)
  in
  let%bind.Or_error current_time = parse_time ~zone forecast.current.time
  and hourly_times = parse_times ~zone forecast.hourly.time
  and daily_times =
    List.map forecast.daily.time ~f:(fun time ->
      Or_error.try_with (fun () -> Date.of_string time))
    |> Or_error.combine_errors
  and sunrises = parse_optional_times ~zone forecast.daily.sunrise
  and sunsets = parse_optional_times ~zone forecast.daily.sunset in
  let%bind.Or_error hourly =
    Or_error.try_with (fun () ->
      let precipitation_conditions_and_uv =
        List.map3_exn
          forecast.hourly.precipitation_probability
          forecast.hourly.weather_code
          forecast.hourly.uv_index
          ~f:(fun precipitation_probability weather_code uv_index ->
            ( precipitation_probability
            , Option.map weather_code ~f:classify_weather_code
            , uv_index ))
      in
      List.map3_exn
        hourly_times
        forecast.hourly.temperature_2m
        precipitation_conditions_and_uv
        ~f:(fun time temperature_2m (precipitation_probability, conditions, uv_index) ->
          { Hourly.time; temperature_2m; precipitation_probability; conditions; uv_index }))
  in
  let%map.Or_error daily =
    Or_error.try_with (fun () ->
      List.map3_exn
        daily_times
        sunrises
        (List.zip_exn sunsets forecast.daily.moon_phase)
        ~f:(fun date sunrise (sunset, moon_phase) ->
          { Daily.date; sunrise; sunset; moon_phase }))
  in
  { Forecast.timezone = forecast.timezone
  ; current =
      { Forecast_current.time = current_time
      ; interval_seconds = forecast.current.interval
      ; temperature_2m = forecast.current.temperature_2m
      ; conditions = Option.map forecast.current.weather_code ~f:classify_weather_code
      ; uv_index = forecast.current.uv_index
      }
  ; hourly
  ; daily
  }
;;

let parse_air_quality (air_quality : Raw.Air_quality.t) =
  let%bind.Or_error zone =
    Or_error.try_with (fun () -> Time_ns_unix.Zone.find_exn air_quality.timezone)
  in
  let%map.Or_error time = parse_time ~zone air_quality.current.time in
  { Air_quality.current =
      { Air_quality_current.time
      ; interval_seconds = air_quality.current.interval
      ; us_aqi = air_quality.current.us_aqi
      }
  }
;;

let fetch_weather ~coordinates () =
  let latitude = coordinates.Coordinates.latitude
  and longitude = coordinates.longitude in
  let%bind.Deferred.Or_error forecast, air_quality =
    Deferred.Or_error.both
      (fetch
         ~url:
           [%string
             "https://api.open-meteo.com/v1/forecast?latitude=%{latitude#Float}&longitude=%{longitude#Float}&current=temperature_2m,weather_code,uv_index&hourly=temperature_2m,precipitation_probability,weather_code,uv_index&daily=sunrise,sunset,moon_phase&timezone=auto&forecast_hours=25"]
         ~decoder:Raw.Forecast.t_of_yojson)
      (fetch
         ~url:
           [%string
             "https://air-quality-api.open-meteo.com/v1/air-quality?latitude=%{latitude#Float}&longitude=%{longitude#Float}&current=us_aqi&timezone=auto"]
         ~decoder:Raw.Air_quality.t_of_yojson)
  in
  return
    (let%bind.Or_error forecast = parse_forecast forecast
     and air_quality = parse_air_quality air_quality in
     Ok (forecast, air_quality))
;;

let query cache ~coordinates =
  Cache.get
    cache
    (module struct
      type t = Forecast.t * Air_quality.t [@@deriving sexp]
    end)
    ~max_age:(Time_ns.Span.of_min 15.)
    ~fetch:(fetch_weather ~coordinates)
    ~key:
      [%string
        "weather_%{coordinates.Coordinates.latitude#Float}_%{coordinates.longitude#Float}"]
;;
