open! Core

module Cloudy_conditions = struct
  type t =
    { rain : bool
    ; snow : bool
    ; thunderstorm : bool
    }
end

module Conditions = struct
  type t =
    | Not_cloudy
    | Cloudy of Cloudy_conditions.t
end

type t =
  { current_temperature_celsius : float option
  ; low_temperature_celsius : float option
  ; high_temperature_celsius : float option
  ; maximum_uv_index : float option
  ; conditions : Conditions.t
  ; moon_phase : float option
  ; sunrise : Time_ns.t
  ; sunset : Time_ns.t
  }

let create ~look_forward_hours ~now ~(forecast : Feeds.Weather.Forecast.t) =
  let forecast_ends_at = Time_ns.add now (Time_ns.Span.of_int_hr look_forward_hours) in
  let hourly_forecasts =
    List.filter forecast.hourly ~f:(fun hourly_forecast ->
      Time_ns.compare hourly_forecast.time now >= 0
      && Time_ns.compare hourly_forecast.time forecast_ends_at <= 0)
  in
  let%bind.Or_error daily =
    match List.hd forecast.daily with
    | Some daily -> Ok daily
    | None -> Or_error.error_string "Open-Meteo returned no daily forecast"
  in
  let%map.Or_error sunrise =
    match daily.sunrise with
    | Some sunrise -> Ok sunrise
    | None -> Or_error.error_string "Open-Meteo returned no sunrise"
  and sunset =
    match daily.sunset with
    | Some sunset -> Ok sunset
    | None -> Or_error.error_string "Open-Meteo returned no sunset"
  in
  let temperatures =
    Option.to_list forecast.current.temperature_2m
    @ List.filter_map hourly_forecasts ~f:(fun forecast -> forecast.temperature_2m)
  in
  let conditions =
    Option.to_list forecast.current.conditions
    @ List.filter_map hourly_forecasts ~f:(fun forecast -> forecast.conditions)
  in
  let rain = List.exists conditions ~f:(fun conditions -> conditions.rain)
  and snow = List.exists conditions ~f:(fun conditions -> conditions.snow)
  and thunderstorm =
    List.exists conditions ~f:(fun conditions -> conditions.thunderstorm)
  in
  let conditions =
    match
      List.exists conditions ~f:(fun conditions -> conditions.cloudy)
      || rain
      || snow
      || thunderstorm
    with
    | true -> Conditions.Cloudy { rain; snow; thunderstorm }
    | false -> Not_cloudy
  in
  { current_temperature_celsius = forecast.current.temperature_2m
  ; low_temperature_celsius = List.min_elt temperatures ~compare:Float.compare
  ; high_temperature_celsius = List.max_elt temperatures ~compare:Float.compare
  ; maximum_uv_index =
      Option.to_list forecast.current.uv_index
      @ List.filter_map hourly_forecasts ~f:(fun forecast -> forecast.uv_index)
      |> List.max_elt ~compare:Float.compare
  ; conditions
  ; moon_phase = daily.moon_phase
  ; sunrise
  ; sunset
  }
;;
