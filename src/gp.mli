open Owl

(**
 The workflow is:
   + define your figure, ie a [(module Plot) -> unit] function
   + decide on an output format (see {!section:out} below)
   + draw your figure: see {!val:draw} below.

  Simple example:
  {[
    open Gp
    let figure (module P: Plot) =
      P.barebone ();
      P.plot (F "cos(x)") ~style:"l lc 8 lw 2"
    let _ = draw ~output:(png "test.png") figure
  ]}
*)

(** {1:prms Gnuplot parameters} *)

type prms

(** @param tmp_root Root directory for temporary files. 
           Default: [/dev/shm] if it exists, otherwise [/tmp].
    @param gnuplot Path to the gnuplot executable.
    @param init A string containing gnuplot commands to be executed right from the beginning. 
           Default is "set key noautotitle; set border 3; set tics out nomirror". *)
val prms : ?tmp_root:string -> ?gnuplot:string -> ?init:string -> unit -> prms


(** {1 Defining your figure} *)

(** {2 Axis types} *)

type axis =
  [ `x (** primary x-axis (bottom border) *)
  | `x2 (** secondary x-axis (top border) *)
  | `y (** primary y-axis (left border) *)
  | `y2 (** secondary y-axis (right border) *)
  | `z (** z-axis (only for 3D plots) *)
  | `cb (** colorbox axis *)
  ]

(** {2 Plot properties} *)

(** These are properties you can set/unset, e.g. 
    {[ let figure (module P: Plot) =
      P.set Title "a beautiful plot";
      P.unset Colorbox;
      P.set Range (`x, (0., Const.pi));
      [...] ]} *)

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

(** {2 Types of data you can plot} *)

type data =
  | A of Mat.mat
      (** An owl array; if it has a single row, it is interpreted as a single column *)
  | L of Mat.mat list
      (** A list of owl arrays, which are concatenated along the second axis
          before being passed to gnuplot. Again, arrays of size [1 x N] are
          interpreted as [N x 1] for convenience *)
  | F of string (** A function as you would write in gnuplot *)

(** {2 Plotting functions} *)

type item

(** See {!val:Plot.plot} for meaning of parameters.
    Example:
    {[ 
    let () =
      let x = Mat.gaussian 100 4 in
      let fig (module P: Plot) =
        P.barebone ();
        P.plots [ item (A x) ~using:"1:3" ~style:"p pt 7 lc 8 ps 0.5";
                  item (A x) ~using:"2:4" ~style:"p pt 7 lc 7 ps 0.5" ] in
      draw ~output:(png "test") fig
    ]} *)
val item : ?using:string -> ?axes:string -> ?style:string -> data -> item

(** Contains all the commands you need to draw your figure *)
module type Plot = sig
  (** Executes an arbitrary gnuplot command. *)
  val ex : string -> unit

  (** Invoke gnuplot's [plot] command on some data.
      @param using In standard gnuplot format, e.g. "1:3" to plot the 3rd
      column against the 1st. Defaults to "0:1" if the data is a single column
      vector, and to "1:2" if the data has at least two columns. 
      @param axes In standard gnuplot format, e.g. "x1y2" for primary x-axis and
      secondary y-axis. Defaults to "x1y1". Example:
      {[ 
         plot (A Mat.(uniform 10 3)) ~using:"1:3" ~style:"lp pt 7 lc 8" 
      ]} *)
  val plot : ?using:string -> ?axes:string -> ?style:string -> data -> unit

  (** Invoke gnuplot's [plot] command on a whole list of data items.
      Example:
      {[
        plots [ item (A Mat.(uniform 10 2)) ~style:"p pt 7 lc 7";
                item (F "cos(x)") ~style:"l lc rgb 'red'" ]
      ]}*)
  val plots : item list -> unit

  (** Same as {!val:plot}, for 3D plots. *)
  val splot : ?using:string -> ?axes:string -> ?style:string -> data -> unit

  (** Same as {!val:plots}, for 3D plots. *)
  val splots : item list -> unit

  val heatmap : ?style:string -> Mat.mat -> unit
  val load : string -> unit

  (** Sets a property. Examples:
    {[ 
      set Logscale `x;
      set Tics (`x, `def (0.0, 10.0, 2.0)) ]} *)
  val set : ?o:string -> 'a property -> 'a -> unit

  (** Unsets a property. Examples:
    {[ 
      unset Logscale `y;
      unset Tics `x ]} *)
  val unset : 'a unset_property -> 'a -> unit

  (** Trim the plot to the bare minimum: no axes, no labels, no tics, nothing but your
      lovely plot. A great place to start for a beautiful plot. *)
  val barebone : unit -> unit

  (** Set the position of your plot's top, bottom, left, and right borders to desired
      positions in "screen coordinates", from (0,0) for bottom left to (1,1) for top
      right. *)
  val margins : [ `t of float | `b of float | `l of float | `r of float ] list -> unit

  (** [multiplot ?rect ?spacing (rows, cols) f] automatically sets the plot
      margins for a [(rows x cols)] tile, and populates the tile by calling
      [f plot_id row_id col_id] (where [plot_id = row_id * cols + col_id],
      ie the function completes the first row first, then the second row, etc.
      You can specify where to position the tile by setting [?rect] to the
      (screen) coordinates of any two opposite corners (default: [((0.1, 0.1),
      (0.9, 0.9))] i.e. 10% padding on all sides). You can also adjust the
      horizontal and vertical spacing between vignettes by setting [?spacing]
      (default: [(0.04, 0.04)] in screen coordinates). 
    
      Example:
      {[
      let data = Mat.gaussian 20 12 (* 12 = 4x3 *)
      let figure (module P: Plot) =
        P.barebone ();
        P.multiplot (4, 3) (fun k row col ->
          P.plot (A Mat.(col data k)) ~style:"p pt 7 lc 8") in
      draw ~output:(png "tile") figure ]} *)
  val multiplot
    :  ?rect:(float * float) * (float * float)
    -> ?spacing:float * float
    -> int * int
    -> (int -> int -> int -> unit)
    -> unit
end

(** {1:out Output terminals} *)

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

(** Main drawing function: takes an output terminal (see {!section:out} above) 
    and a figure. *)
val draw : ?prms:prms -> output:output -> ((module Plot) -> unit) -> unit

(** This is a shorthand for [draw ~output:(qt ())];
    @param interactive (Default: false) Enables interactions with the QT windows, 
           including zooming, etc. Your program will stall until you close the
           QT window though.
    @param size (Default: 600 x 400 pixels) Should be given in (y, x) pixel format. *)
val interactive
  :  ?interactive:bool
  -> ?size:int * int
  -> ((module Plot) -> unit)
  -> unit
