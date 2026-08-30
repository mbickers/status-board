open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type discovery_feed =
  { name : string
  ; url : string
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type discovery_language = { feeds : discovery_feed list }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type discovery_data = { en : discovery_language }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type discovery =
  { data : discovery_data
  ; last_updated : int
  ; ttl : int
  ; version : string
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type station_information =
  { station_id : string
  ; name : string
  ; lon : float
  ; lat : float
  ; capacity : int
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type station_information_data = { stations : station_information list }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type station_information_feed =
  { data : station_information_data
  ; last_updated : int
  ; ttl : int
  ; version : string
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type station_status =
  { station_id : string
  ; num_bikes_available : int
  ; num_ebikes_available : int
  ; num_bikes_disabled : int
  ; num_docks_available : int
  ; num_docks_disabled : int
  ; is_installed : int
  ; is_renting : int
  ; is_returning : int
  ; last_reported : int
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type station_status_data = { stations : station_status list }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type station_status_feed =
  { data : station_status_data
  ; last_updated : int
  ; ttl : int
  ; version : string
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let decode decoder contents =
  let open Or_error.Let_syntax in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string contents) in
  Or_error.try_with (fun () -> decoder json)
;;

let fetch_url url decoder =
  let open Deferred.Or_error.Let_syntax in
  let%bind uri = Or_error.try_with (fun () -> Uri.of_string url) |> Deferred.return in
  let%bind response, body =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
  in
  let%bind contents =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Body.to_string body)
  in
  let status_code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  if Cohttp.Code.is_success status_code
  then decode decoder contents |> Deferred.return
  else
    Deferred.Or_error.errorf
      "GBFS request to %s failed with HTTP %d: %s"
      url
      status_code
      contents
;;

module Feed = struct
  type 'a t =
    { name : string
    ; decoder : Yojson.Safe.t -> 'a
    }

  let station_information =
    { name = "station_information"; decoder = station_information_feed_of_yojson }
  ;;

  let station_status =
    { name = "station_status"; decoder = station_status_feed_of_yojson }
  ;;
end

type t = { feed_urls : string String.Map.t }

let discover url =
  let open Deferred.Or_error.Let_syntax in
  let%bind discovery = fetch_url url discovery_of_yojson in
  let data = discovery.data in
  let%map feed_urls =
    data.en.feeds
    |> List.map ~f:(fun feed -> feed.name, feed.url)
    |> String.Map.of_alist_or_error
    |> Deferred.return
  in
  { feed_urls }
;;

let fetch t (feed : _ Feed.t) =
  match Map.find t.feed_urls feed.name with
  | Some url -> fetch_url url feed.decoder
  | None -> Deferred.Or_error.errorf "GBFS discovery response has no %s feed" feed.name
;;
