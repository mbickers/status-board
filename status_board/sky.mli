open! Core

val draw_sun_moon
  :  Drawing.Context.t
  -> light_fill:Drawing.Primitives.Fill.t
  -> dark_fill:Drawing.Primitives.Fill.t
  -> is_night:bool
  -> weather:Weather_info.t
  -> font:Drawing.Font.t
  -> text_fill:Drawing.Primitives.Fill.t
  -> status_text_size:float
  -> center:int * int
  -> radius:int
  -> unit

val draw_cloud
  :  Drawing.Context.t
  -> fill:Drawing.Primitives.Fill.t
  -> graph_style:Drawing.Graph.Style.t
  -> zone:Time_ns_unix.Zone.t
  -> padding:int
  -> center_x:int
  -> base_y:int
  -> Weather_info.Precipitation.t option
  -> unit
