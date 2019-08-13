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
      P.plot (S "cos(x)") ~style:"l lc 8 lw 2" [ barebone ]
    let () = draw ~output:(png "test.png") figure
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

(** {2 Plot properties } *)

type side =
  [ `left (** left *)
  | `right (** right *)
  | `top (** top *)
  | `bottom (** bottom *)
  ]

type margin =
  [ `left of float (** left *)
  | `right of float (** right *)
  | `top of float (** top *)
  | `bottom of float (** bottom *)
  ]

type offset =
  [ `left of [ `graph of float | `first of float ]
  | `right of [ `graph of float | `first of float ]
  | `top of [ `graph of float | `first of float ]
  | `bottom of [ `graph of float | `first of float ]
  ]

type tics =
  [ `auto (** let gnuplot take care of it *)
  | `manual of (float * string) list (** manual list *)
  | `regular of float * float * float (** start, incr, end *)
  ]

type property

val set : string -> property
val unset : string -> property

(** Trim the plot to the bare minimum: no axes, no labels, no tics, nothing but your
      lovely plot. A great place to start for a beautiful plot. *)
val barebone : property

val title : ?o:string -> string -> property
val margins : margin list -> property
val offsets : offset list -> property
val borders : ?o:string -> side list -> property
val tics : string -> property

val xtics
  :  ?o:string
  -> [ `auto | `manual of (float * string) list | `regular of float list ]
  -> property

val ytics
  :  ?o:string
  -> [ `auto | `manual of (float * string) list | `regular of float list ]
  -> property

val ztics
  :  ?o:string
  -> [ `auto | `manual of (float * string) list | `regular of float list ]
  -> property

val cbtics
  :  ?o:string
  -> [ `auto | `manual of (float * string) list | `regular of float list ]
  -> property

val x2tics
  :  ?o:string
  -> [ `auto | `manual of (float * string) list | `regular of float list ]
  -> property

val y2tics
  :  ?o:string
  -> [ `auto | `manual of (float * string) list | `regular of float list ]
  -> property

val xlabel : ?o:string -> string -> property
val ylabel : ?o:string -> string -> property
val zlabel : ?o:string -> string -> property
val cblabel : ?o:string -> string -> property
val x2label : ?o:string -> string -> property
val y2label : ?o:string -> string -> property
val xrange : ?o:string -> float * float -> property
val yrange : ?o:string -> float * float -> property
val zrange : ?o:string -> float * float -> property
val cbrange : ?o:string -> float * float -> property
val x2range : ?o:string -> float * float -> property
val y2range : ?o:string -> float * float -> property

(** Some decent default plot properties. *)
val default_props : property list

(** {2 Types of data you can plot} *)

type data =
  | A of Mat.mat
      (** An owl array; if it has a single row, it is interpreted as a single column *)
  | L of Mat.mat list
      (** A list of owl arrays, which are concatenated along the second axis
          before being passed to gnuplot. Again, arrays of size [1 x N] are
          interpreted as [N x 1] for convenience *)
  | F of ((float -> float) * Mat.mat) (** A function f(x) over some x-range given as a 1xN or Nx1 matrix *)
  | S of string (** A function as you would write in gnuplot *)

(** {2 Plotting functions} *)

type item

(** See {!val:Plot.plot} for meaning of parameters.
    Example:
    {[ 
    let () =
      let x = Mat.gaussian 100 4 in
      let fig (module P: Plot) =
        P.plots 
          [ item (A x) ~using:"1:3" ~style:"p pt 7 lc 8 ps 0.5";
            item (A x) ~using:"2:4" ~style:"p pt 7 lc 7 ps 0.5" ]
          [ barebone ] in
      draw ~output:(png "test") fig
    ]} *)
val item
  :  ?using:string
  -> ?axes:string
  -> ?legend:string
  -> ?style:string
  -> data
  -> item

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
         plot (A Mat.(uniform 10 3)) ~using:"1:3" ~style:"lp pt 7 lc 8" [ ... properties ... ]
      ]} *)
  val plot
    :  ?using:string
    -> ?axes:string
    -> ?legend:string
    -> ?style:string
    -> data
    -> property list
    -> unit

  (** Invoke gnuplot's [plot] command on a whole list of data items.
      Example:
      {[
        plots [ item (A Mat.(uniform 10 2)) ~style:"p pt 7 lc 7";
                item (F "cos(x)") ~style:"l lc rgb 'red'" ] [ ...properties... ]
      ]}*)
  val plots : item list -> property list -> unit

  (** Same as {!val:plot}, for 3D plots. *)
  val splot
    :  ?using:string
    -> ?axes:string
    -> ?legend:string
    -> ?style:string
    -> data
    -> property list
    -> unit

  (** Same as {!val:plots}, for 3D plots. *)
  val splots : item list -> property list -> unit

  val heatmap : ?style:string -> Mat.mat -> property list -> unit
  val load : string -> unit

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
        P.multiplot (4, 3) (fun k row col ->
          P.plot (A Mat.(col data k)) ~style:"p pt 7 lc 8" [ barebone ]) in
      draw ~output:(png "tile") figure ]} *)
  val multiplot
    :  ?rect:(float * float) * (float * float)
    -> ?spacing:float * float
    -> int * int
    -> (int -> int -> int -> unit)
    -> unit
end

(** {1:out Output terminals} *)

(** NB: if you are in a Jupyter notebook, you should not use any of the terminals below;
    instead, use {!val:Juplot.draw}. *)

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

type tikz_font

val cmbright : tikz_font
val helvetica : tikz_font
val tikz : ?grid:bool -> ?crop:bool -> ?font:tikz_font -> ?tex:string -> string -> output

(** {1 Drawing your figure} *)

(** Main drawing function: takes an output terminal (see {!section:out} above) 
    and a figure. *)
val draw : ?prms:prms -> output:output -> ((module Plot) -> unit) -> unit

(** This is a shorthand for [draw ~output:(qt ())];
    @param interactive (Default: false) Enables interactions with the QT windows, 
           including zooming, etc. Your program will stall until you close the
           QT window though.
    @param size (Default: 600 x 400 pixels) Should be given in (y, x) pixel format. *)
val interactive : ?interactive:bool -> ?size:int * int -> ((module Plot) -> unit) -> unit
