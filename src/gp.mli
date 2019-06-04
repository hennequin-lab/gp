open Owl

(** {[

open Gp
let my_figure (module F: Figure) =
  F.barebone ();
  F.ex "plot cos(x) w l lc 8 lw 2"
let _ = draw ~output:(png "test.png") my_figure

]}
*)

(** {1 Gnuplot parameters} *)

type prms

(** @param tmp_root Root directory for temporary files. Default: [/dev/shm] if it exists, otherwise [/tmp].
    @param gnuplot Path to the gnuplot executable.
    @param init A string containing gnuplot commands to be executed right from the beginning. Default
                is "set key noautotitle; set border 3; set tics out nomirror" *)
val prms : ?tmp_root:string -> ?gnuplot:string -> ?init:string -> unit -> prms

(** {1 Output terminals} *)

type output

val svg : ?font:string -> ?size:int * int -> ?other_term_opts:string -> string -> output
val png : ?font:string -> ?size:int * int -> ?other_term_opts:string -> string -> output

val qt
  :  ?font:string
  -> ?size:int * int
  -> ?other_term_opts:string
  -> ?pause:string
  -> unit
  -> output

val latex : ?term_opts:string -> string -> output

(** {1 Drawing your figure} *)

(** Axis types *)
type axis =
  [ `x (** primary x-axis (bottom border) *)
  | `x2 (** secondary x-axis (top border) *)
  | `y (** primary y-axis (left border) *)
  | `y2 (** secondary y-axis (right border) *)
  | `z (** z-axis (only for 3D plots) *)
  | `cb (** colorbox axis *)
  ]

(** {2 Plot properties that can be set / unset} *)

type _ property =
  | Title : string property
  | Label : (axis * string) property
  | Range : (axis * (float * float)) property
  | Tics
      : (axis * [ `list of (float * string) list | `def of float * float * float ])
        property
  | Key : string property
  | Palette : string property
  | Format : (axis * string) property
  | Autoscale : axis property
  | Logscale : axis property
  | Text : (int * string) property
  | Border : [ `t | `b | `l | `r ] list property
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

type data =
  | A of Mat.mat
  | L of Mat.mat list
  | F of string

type item
  
(** Example:
    {[ 
    let () =
      let x = Mat.gaussian 100 4 in
      let fig (module F) =
        F.barebone ();
        F.plots [ item (A x) ~using:"1:3" ~style:"p pt 7 lc 8 ps 0.5";
                  item (A x) ~using:"2:4" ~style:"p pt 7 lc 7 ps 0.5" ] in
      draw ~output:(png "test") fig
    ]} *)
val item : ?using:string -> ?axes:string -> ?style:string -> data -> item

(** Contains all the commands you need to draw your figure *)
module type Figure = sig
  (** Execute an arbitrary gnuplot command *)
  val ex : string -> unit

  val plot : ?using:string -> ?axes:string -> ?style:string -> data -> unit
  val plots : item list -> unit
  val splot : ?using:string -> ?axes:string -> ?style:string -> data -> unit
  val splots : item list -> unit
  val heatmap : ?style:string -> Mat.mat -> unit
  val load : string -> unit
  val set : ?o:string -> 'a property -> 'a -> unit
  val unset : 'a unset_property -> 'a -> unit

  (** Trim the plot to the bare minimum: no axes, no labels, no tics, nothing but your
      lovely plot *)
  val barebone : unit -> unit

  (** Set the position of your plot's top, bottom, left, and right borders to desired
      positions in "screen coordinates", from (0,0) for bottom left to (1,1) for top
      right. *)
  val margins : [ `t of float | `b of float | `l of float | `r of float ] list -> unit

  val multiplot
    :  ?rect:(float * float) * (float * float)
    -> ?spacing:float * float
    -> int * int
    -> (int -> int -> int -> unit)
    -> unit
end

(** Main drawing function *)
val draw : ?prms:prms -> output:output -> ((module Figure) -> unit) -> unit

(** [interactive] (default: false) enables interactions with the QT windows, including
    zooming, etc. Your program will stall until you close the QT window though.
    [size] (default: 600 x 400 pixels) should be given in (y, x) pixel format. *)
val interactive
  :  ?interactive:bool
  -> ?size:int * int 
  -> ((module Figure) -> unit)
  -> unit
