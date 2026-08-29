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

let decode decoder contents = contents |> Yojson.Safe.from_string |> decoder

let find_feed_url (discovery : discovery) name =
  let data : discovery_data = discovery.data in
  data.en.feeds
  |> List.find ~f:(fun (feed : discovery_feed) -> String.equal feed.name name)
  |> Option.value_exn
  |> fun (feed : discovery_feed) -> feed.url
;;

let fetch url decoder =
  let open Deferred.Let_syntax in
  let%bind response, body = Cohttp_async.Client.get (Uri.of_string url) in
  let%map contents = Cohttp_async.Body.to_string body in
  let status_code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  if Cohttp.Code.is_success status_code
  then decode decoder contents
  else failwithf "GBFS request to %s failed with HTTP %d: %s" url status_code contents ()
;;
