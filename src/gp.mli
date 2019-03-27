(* ----------------------------------------------------------------------------
   --    Output terminals                                                    --
   ---------------------------------------------------------------------------- *)

type term = {term: string; font: string option; size: (int * int) option; other: string option}

val opts_of : term -> string

module type Output = sig
  val term : term
  val file_ext : string
  val post_action : (string -> unit) option
  (* possibly do something with the root filename after "draw" *)
end

module SVG : Output
module PNG : Output
module QT : Output
module LaTeX : Output
(* ----------------------------------------------------------------------------
   --    Main gnuplot types and module                                       --
   ---------------------------------------------------------------------------- *)

(* properties to be set / unset *)

(** Type of axis you can use in various other commands *)
type axis = [`x | `x2 | `y | `y2 | `z | `cb]

type _ property =
  | Title : string property
  | Label : (axis * string) property
  | Range : (axis * (float * float)) property
  | Tics : (axis * [`list of (float * string) list | `def of float * float * float]) property
  | Key : string property
  | Palette : string property
  | Format : (axis * string) property
  | Autoscale : axis property
  | Logscale : axis property
  | Text : (int * string) property
  | Border : [`t | `b | `l | `r] list property
  | Colorbox : string property
  | Multiplot : unit property
  | Prop : string property

type _ unset_property =
  | Title : unit unset_property
  | Label : axis unset_property
  | Tics : axis unset_property
  | Key : unit unset_property
  | Autoscale : axis unset_property
  | Logscale : axis unset_property
  | Text : int unset_property
  | Border : unit unset_property
  | Colorbox : unit unset_property
  | Multiplot : unit unset_property
  | Prop : string unset_property

(** Contains all the commands you need to draw your figure *)
module type Figure = sig
  val h : out_channel
  val ex : string -> unit
  val draw : unit -> unit
  val send_columns : Owl.Mat.mat array -> unit
  val plot : (Owl.Mat.mat list * string) array -> unit
  val splot : Owl.Mat.mat * string -> unit
  val image : Owl.Mat.mat -> unit
  val load : string -> unit
  val set : ?o:string -> 'a property -> 'a -> unit
  val unset : 'a unset_property -> 'a -> unit

  val barebone : unit -> unit
  (** Trim the plot to the bare minimum: 
      no axes, no labels, no tics, nothing but your lovely plot *)

  val margins : [`t of float | `b of float | `l of float | `r of float] list -> unit

  val multiplot :
       ?rect:(float * float) * (float * float)
    -> ?spacing:float * float
    -> int * int
    -> (int -> int -> int -> unit)
    -> unit
end

val default_init : string

val figure :
  ?gnuplot:string -> ?init:string -> ?to_file:string -> (module Output) -> (module Figure)

val quick : ?size:int * int -> ((module Figure) -> unit) -> unit
