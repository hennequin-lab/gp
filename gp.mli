open Printf
open Owl

(* ----------------------------------------------------------------------------
   --    Output terminals                                                    --
   ---------------------------------------------------------------------------- *)

module type Output = sig
  val term: string (* applied as is after `set terminal ...` *)
  val term_opts: string list
  val file_ext: string
  val post_action: (string -> unit) option (* possibly do something with the root filename after "draw" *)
end

module SVG : Output
module PNG : Output
module QT : Output
module LaTeX : Output


(* ----------------------------------------------------------------------------
   --    Figure parameters                                                   --
   ---------------------------------------------------------------------------- *)

module type Parameters = sig
  val gnuplot: string
  val init: string
  val to_file: string option
end

val default_init: string


(* ----------------------------------------------------------------------------
   --    Main gnuplot types and module                                       --
   ---------------------------------------------------------------------------- *)

(* properties to be set / unset *)

type axis = [ `x | `x2 | `y | `y2 | `z | `cb ]

type _ property =
  | Title       : string property
  | Label       : (axis * string) property
  | Range       : (axis * (float * float)) property
  | Tics        : (axis * ([ `list of (float * string) list 
                           | `def of (float * float * float)])) property
  | Key         : string property
  | Palette     : string property
  | Format      : (axis * string) property
  | Autoscale   : axis property
  | Logscale    : axis property
  | Text        : (int * string) property
  | Border      : [ `t | `b | `l | `r ] list property
  | Colorbox    : string property
  | Multiplot   : unit property
  | Prop        : string property

type _ unset_property =
  | Title       : unit unset_property
  | Label       : axis unset_property
  | Tics        : axis unset_property
  | Key         : unit unset_property
  | Autoscale   : axis unset_property
  | Logscale    : axis unset_property
  | Text        : int unset_property
  | Border      : unit unset_property
  | Colorbox    : unit unset_property
  | Multiplot   : unit unset_property
  | Prop        : string unset_property


module type Figure = sig
  val h: out_channel
  val ex: string -> unit
  val draw: unit -> unit
  val send_columns: Mat.mat array -> unit
  val plot: (Mat.mat list * string) array -> unit
  val splot: (Mat.mat * string) -> unit
  val image: Mat.mat -> unit
  val load: string -> unit
  val set: ?o:string -> 'a property -> 'a -> unit
  val unset: 'a unset_property -> 'a -> unit
  val margins: [ `t of float | `b of float | `l of float | `r of float ] list -> unit
  val multiplot: int -> int -> float -> float -> (int -> int -> int -> unit) -> unit
end


(* main module *)
module New_figure (O: Output) (P: Parameters) : Figure


val figure: ?gnuplot:string -> ?init:string -> ?to_file:string -> (module Output) -> (module Figure)

val quick : unit -> (module Figure)

