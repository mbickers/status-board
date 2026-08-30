open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Discovery_feed = struct
  type t =
    { name : string
    ; url : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Discovery_language = struct
  type t = { feeds : Discovery_feed.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Discovery_data = struct
  type t = { en : Discovery_language.t } [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Discovery = struct
  type t =
    { data : Discovery_data.t
    ; last_updated : int
    ; ttl : int
    ; version : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Station_information = struct
  type t =
    { station_id : string
    ; name : string
    ; lon : float
    ; lat : float
    ; capacity : int
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Station_information_data = struct
  type t = { stations : Station_information.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Station_information_feed = struct
  type t =
    { data : Station_information_data.t
    ; last_updated : int
    ; ttl : int
    ; version : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Station_status = struct
  type t =
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
end

module Station_status_data = struct
  type t = { stations : Station_status.t list }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Station_status_feed = struct
  type t =
    { data : Station_status_data.t
    ; last_updated : int
    ; ttl : int
    ; version : string
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

let decode decoder contents =
  let%bind.Or_error json =
    Or_error.try_with (fun () -> Yojson.Safe.from_string contents)
  in
  Or_error.try_with (fun () -> decoder json)
;;

let fetch_url url decoder =
  let%bind.Deferred.Or_error uri =
    Or_error.try_with (fun () -> Uri.of_string url) |> return
  in
  let%bind.Deferred.Or_error response, body =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
  in
  let%bind.Deferred.Or_error contents =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Body.to_string body)
  in
  let status_code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  if Cohttp.Code.is_success status_code
  then decode decoder contents |> return
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
    { name = "station_information"; decoder = Station_information_feed.t_of_yojson }
  ;;

  let station_status =
    { name = "station_status"; decoder = Station_status_feed.t_of_yojson }
  ;;
end

module Discovered_endpoints = struct
  type t = { feed_urls : string String.Map.t }
end

let discover url =
  let%bind.Deferred.Or_error discovery = fetch_url url Discovery.t_of_yojson in
  return
    (discovery.data.en.feeds
     |> List.map ~f:(fun feed -> feed.name, feed.url)
     |> String.Map.of_alist_or_error
     |> Or_error.map ~f:(fun feed_urls -> { Discovered_endpoints.feed_urls }))
;;

let fetch discovered_endpoints (feed : _ Feed.t) =
  match Map.find discovered_endpoints.Discovered_endpoints.feed_urls feed.name with
  | Some url -> fetch_url url feed.decoder
  | None -> Deferred.Or_error.errorf "GBFS discovery response has no %s feed" feed.name
;;
