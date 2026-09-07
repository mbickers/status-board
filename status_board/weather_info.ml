open! Core

module Precipitation = struct
  module Sample = struct
    type t =
      { time : Time_ns.t
      ; probability_frac : float option
      }
  end

  type t =
    { precipitation : Feeds.Weather.Precipitation.t
    ; samples : Sample.t list
    }
end

module Conditions = struct
  type t =
    | Not_cloudy
    | Cloudy of Precipitation.t option
end

type t =
  { current_temperature_celsius : float option
  ; low_temperature_celsius : float option
  ; high_temperature_celsius : float option
  ; maximum_uv_index : float option
  ; us_aqi : float option
  ; conditions : Conditions.t
  ; moon_phase : float option
  ; sunrise : Time_ns.t
  ; sunset : Time_ns.t
  }

let create ~look_forward_hours ~now ~(forecast : Feeds.Weather.Forecast.t) ~us_aqi =
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
    @ List.filter_map hourly_forecasts ~f:(fun hourly -> hourly.conditions)
  in
  let precipitation =
    List.filter_map conditions ~f:(function
      | Feeds.Weather.Conditions.Cloudy precipitation -> precipitation
      | Not_cloudy -> None)
    |> List.reduce ~f:(fun combined precipitation ->
      let kind =
        match combined.kind, precipitation.kind with
        | Rain, Rain -> Feeds.Weather.Precipitation.Kind.Rain
        | Snow, Snow -> Snow
        | _ -> Rain_and_snow
      in
      { Feeds.Weather.Precipitation.kind
      ; thunder = combined.thunder || precipitation.thunder
      })
    |> Option.map ~f:(fun precipitation ->
      let samples =
        List.filter_map forecast.hourly ~f:(fun hourly ->
          match
            Time_ns.compare hourly.time now > 0
            && Time_ns.compare hourly.time forecast_ends_at <= 0
          with
          | false -> None
          | true ->
            Some
              { Precipitation.Sample.time = hourly.time
              ; probability_frac =
                  Option.map
                    hourly.preceding_hour_precipitation_probability
                    ~f:(fun probability -> Float.of_int probability /. 100.)
              })
        |> List.sort ~compare:(fun left right -> Time_ns.compare left.time right.time)
      in
      let samples =
        List.fold_right samples ~init:[] ~f:(fun sample rest ->
          match rest with
          | [] -> [ sample ]
          | next :: _ ->
            let hours = Time_ns.diff next.time sample.time |> Time_ns.Span.to_hr in
            let missing =
              List.range 1 (Float.iround_up_exn hours)
              |> List.map ~f:(fun hour ->
                { Precipitation.Sample.time =
                    Time_ns.add sample.time (Time_ns.Span.of_int_hr hour)
                ; probability_frac = None
                })
            in
            (sample :: missing) @ rest)
      in
      { Precipitation.precipitation; samples })
  in
  let conditions =
    match precipitation with
    | Some precipitation -> Conditions.Cloudy (Some precipitation)
    | None ->
      (match
         List.exists conditions ~f:(function
           | Feeds.Weather.Conditions.Cloudy _ -> true
           | Not_cloudy -> false)
       with
       | true -> Conditions.Cloudy None
       | false -> Not_cloudy)
  in
  { current_temperature_celsius = forecast.current.temperature_2m
  ; low_temperature_celsius = List.min_elt temperatures ~compare:Float.compare
  ; high_temperature_celsius = List.max_elt temperatures ~compare:Float.compare
  ; maximum_uv_index =
      Option.to_list forecast.current.uv_index
      @ List.filter_map hourly_forecasts ~f:(fun forecast -> forecast.uv_index)
      |> List.max_elt ~compare:Float.compare
  ; us_aqi
  ; conditions
  ; moon_phase = daily.moon_phase
  ; sunrise
  ; sunset
  }
;;

let%expect_test
    "hourly conditions use their timestamps and precipitation preserves missing \
     probabilities"
  =
  let at hours = Time_ns.add Time_ns.epoch (Time_ns.Span.of_hr hours) in
  let now = at 10.5 in
  let check ~now ~wet_hours ~missing_hours ~missing_probabilities =
    let forecast =
      { Feeds.Weather.Forecast.timezone = "UTC"
      ; current =
          { time = now
          ; interval_seconds = 900
          ; temperature_2m = None
          ; conditions = None
          ; uv_index = None
          }
      ; hourly =
          List.range 10 20
          |> List.filter ~f:(fun hour ->
            not (List.mem missing_hours hour ~equal:Int.equal))
          |> List.map ~f:(fun hour ->
            { Feeds.Weather.Hourly.time = at (Float.of_int hour)
            ; temperature_2m = None
            ; preceding_hour_precipitation_probability =
                (match List.mem missing_probabilities hour ~equal:Int.equal with
                 | true -> None
                 | false -> Some ((hour - 10) * 10))
            ; conditions =
                Some
                  (match List.mem wet_hours hour ~equal:Int.equal with
                   | false -> Feeds.Weather.Conditions.Not_cloudy
                   | true -> Cloudy (Some { kind = Rain; thunder = false }))
            ; uv_index = None
            })
      ; daily =
          [ { date = Date.of_string "1970-01-01"
            ; sunrise = Some (at 6.)
            ; sunset = Some (at 20.)
            ; moon_phase = None
            }
          ]
      }
    in
    match create ~look_forward_hours:8 ~now ~forecast ~us_aqi:None with
    | Error error -> print_s [%sexp (error : Error.t)]
    | Ok { conditions = Not_cloudy; _ } -> print_endline "clear"
    | Ok { conditions = Cloudy None; _ } -> print_endline "cloudy"
    | Ok { conditions = Cloudy (Some precipitation); _ } ->
      let hours time = Time_ns.diff time Time_ns.epoch |> Time_ns.Span.to_hr in
      let samples =
        List.map precipitation.samples ~f:(fun sample ->
          hours sample.time, sample.probability_frac)
      in
      print_s [%sexp (samples : (float * float option) list)]
  in
  check ~now ~wet_hours:[ 10 ] ~missing_hours:[] ~missing_probabilities:[];
  check ~now ~wet_hours:[ 12 ] ~missing_hours:[ 13 ] ~missing_probabilities:[ 14 ];
  check ~now ~wet_hours:[ 19 ] ~missing_hours:[] ~missing_probabilities:[];
  check ~now:(at 10.) ~wet_hours:[ 18 ] ~missing_hours:[] ~missing_probabilities:[];
  check ~now:(at 10.) ~wet_hours:[ 10 ] ~missing_hours:[] ~missing_probabilities:[];
  [%expect
    {|
    clear
    ((11 (0.1)) (12 (0.2)) (13 ()) (14 ()) (15 (0.5)) (16 (0.6)) (17 (0.7))
     (18 (0.8)))
    clear
    ((11 (0.1)) (12 (0.2)) (13 (0.3)) (14 (0.4)) (15 (0.5)) (16 (0.6)) (17 (0.7))
     (18 (0.8)))
    ((11 (0.1)) (12 (0.2)) (13 (0.3)) (14 (0.4)) (15 (0.5)) (16 (0.6)) (17 (0.7))
     (18 (0.8)))
    |}]
;;

module Testing_data = struct
  let celsius_of_fahrenheit fahrenheit = (fahrenheit -. 32.) *. 5. /. 9.

  let dense_text ~now ~zone =
    { current_temperature_celsius = Some (celsius_of_fahrenheit 104.)
    ; low_temperature_celsius = Some (celsius_of_fahrenheit 99.)
    ; high_temperature_celsius = Some (celsius_of_fahrenheit 109.)
    ; maximum_uv_index = Some 10.
    ; us_aqi = Some 499.
    ; conditions =
        Conditions.Cloudy
          (Some
             { precipitation = { kind = Snow; thunder = false }
             ; samples =
                 List.init 9 ~f:(fun hour ->
                   { Precipitation.Sample.time =
                       Time_ns.add now (Time_ns.Span.of_int_hr hour)
                   ; probability_frac = Some 1.
                   })
             })
    ; moon_phase = Some 0.7
    ; sunrise =
        Time_ns.occurrence
          `First_after_or_at
          now
          ~ofday:(Time_ns.Ofday.create ~hr:6 ())
          ~zone
    ; sunset =
        Time_ns.occurrence
          `First_after_or_at
          now
          ~ofday:(Time_ns.Ofday.create ~hr:20 ())
          ~zone
    }
  ;;

  let stormy ~hour_start ~zone =
    let now = Time_ns.add hour_start (Time_ns.Span.of_min 30.) in
    let hourly =
      List.init 10 ~f:(fun hour ->
        let precipitation =
          Some { Feeds.Weather.Precipitation.kind = Rain_and_snow; thunder = true }
        in
        let preceding_hour_precipitation_probability =
          match hour with
          | 3 -> None
          | _ -> Some (100 - (Int.abs (4 - hour) * 20))
        in
        { Feeds.Weather.Hourly.time = Time_ns.add hour_start (Time_ns.Span.of_int_hr hour)
        ; temperature_2m = Some (celsius_of_fahrenheit 32.)
        ; preceding_hour_precipitation_probability
        ; conditions = Some (Cloudy precipitation)
        ; uv_index = None
        })
    in
    let forecast =
      { Feeds.Weather.Forecast.timezone = Time_ns_unix.Zone.to_string zone
      ; current =
          { time = now
          ; interval_seconds = 900
          ; temperature_2m = Some (celsius_of_fahrenheit 32.)
          ; conditions = None
          ; uv_index = None
          }
      ; hourly
      ; daily =
          [ { date = Time_ns.to_date now ~zone
            ; sunrise =
                Time_ns.occurrence
                  `First_after_or_at
                  now
                  ~ofday:(Time_ns.Ofday.create ~hr:6 ())
                  ~zone
                |> Option.some
            ; sunset =
                Time_ns.occurrence
                  `First_after_or_at
                  now
                  ~ofday:(Time_ns.Ofday.create ~hr:20 ())
                  ~zone
                |> Option.some
            ; moon_phase = Some 0.7
            }
          ]
      }
    in
    create ~look_forward_hours:8 ~now ~forecast ~us_aqi:(Some 123.)
  ;;

  let errors ~now ~zone =
    { current_temperature_celsius = None
    ; low_temperature_celsius = None
    ; high_temperature_celsius = None
    ; maximum_uv_index = None
    ; us_aqi = None
    ; conditions = Conditions.Not_cloudy
    ; moon_phase = None
    ; sunrise =
        Time_ns.occurrence
          `First_after_or_at
          now
          ~ofday:(Time_ns.Ofday.create ~hr:6 ())
          ~zone
    ; sunset =
        Time_ns.occurrence
          `First_after_or_at
          now
          ~ofday:(Time_ns.Ofday.create ~hr:20 ())
          ~zone
    }
  ;;
end
