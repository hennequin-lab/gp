open Printf
open Owl

type prms =
  { tmp_root : string
  ; gnuplot : string
  ; init : string
  }

let prms
    ?tmp_root
    ?(gnuplot = "gnuplot")
    ?(init = "set key noautotitle; set border 3; set tics out nomirror")
    ()
  =
  let tmp_root =
    match tmp_root with
    | Some r -> r
    | None -> if Sys.is_directory "/dev/shm" then "/dev/shm" else "tmp"
  in
  { tmp_root; gnuplot; init }


let default_prms = prms ()

type term =
  { term : string
  ; font : string option
  ; size : (int * int) option
  ; other : string option
  }

let opts_of z =
  let s = z.term in
  let s =
    match z.other with
    | Some x -> sprintf "%s %s" s x
    | None -> s
  in
  let s =
    match z.font with
    | Some x -> sprintf "%s font '%s'" s x
    | None -> s
  in
  let s =
    match z.size with
    | Some (x, y) -> sprintf "%s size %i,%i" s x y
    | None -> s
  in
  s


let ensure_ext ext name =
  let name = Fpath.(v name) in
  (if Fpath.has_ext ext name then name else Fpath.add_ext ext name) |> Fpath.to_string


type output =
  { term : term
  ; file : string option
  ; pause : string option
  ; (* possibly do something with the root filename after "draw" *)
    post_action : (string -> unit) option
  }

let svg ?(font = "Helvetica,12") ?(size = 600, 400) ?other_term_opts file_name =
  { term = { term = "svg"; font = Some font; size = Some size; other = other_term_opts }
  ; file = Some (ensure_ext "svg" file_name)
  ; pause = None
  ; post_action = None
  }


let png
    ?(font = "Helvetica,10")
    ?(size = 600, 400)
    ?(other_term_opts = "enhanced color notransparent crop")
    file_name
  =
  { term =
      { term = "pngcairo"
      ; other = Some other_term_opts
      ; font = Some font
      ; size = Some size
      }
  ; file = Some (ensure_ext "png" file_name)
  ; pause = None
  ; post_action = None
  }


let qt
    ?(font = "Helvetica,10")
    ?(size = 600, 400)
    ?(other_term_opts = "enhanced persist raise")
    ?pause
    ()
  =
  { term =
      { term = "qt"; font = Some font; size = Some size; other = Some other_term_opts }
  ; file = None
  ; pause
  ; post_action = None
  }


let latex_default_opts =
  {| 
    pdf standalone size 100cm, 100cm dl 0.5 header \
    '\usepackage[scaled=1]{helvet} \
     \usepackage{sfmath, xcolor} \
     \renewcommand{\familydefault}{\sfdefault}' |}


let latex ?(term_opts = latex_default_opts) file_name =
  { term = { term = "cairolatex"; size = None; font = None; other = Some term_opts }
  ; file = Some (ensure_ext "tex" file_name)
  ; pause = None
  ; post_action = Some (fun root -> ignore (Sys.command (sprintf "pdflatex %s.tex" root)))
  }


type axis =
  [ `x
  | `x2
  | `y
  | `y2
  | `z
  | `cb
  ]

let string_of_axis = function
  | `x -> "x"
  | `x2 -> "x2"
  | `y -> "y"
  | `y2 -> "y2"
  | `z -> "z"
  | `cb -> "cb"


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
  | F of (float -> float)

(** Contains all the commands you need to draw your figure *)
module type Figure = sig
  val ex : string -> unit
  val plot : (data * string) list -> unit
  val splot : (data * string) list -> unit
  val heatmap : Mat.mat -> unit
  val load : string -> unit
  val set : ?o:string -> 'a property -> 'a -> unit
  val unset : 'a unset_property -> 'a -> unit

  (** Trim the plot to the bare minimum: no axes, no labels, no tics, nothing but your
      lovely plot *)
  val barebone : unit -> unit

  (** Set the plot margins in screen coordinates; (0,0) = bottom left, (1,1) = top right *)
  val margins : [ `t of float | `b of float | `l of float | `r of float ] list -> unit

  val multiplot
    :  ?rect:(float * float) * (float * float)
    -> ?spacing:float * float
    -> int * int
    -> (int -> int -> int -> unit)
    -> unit
end

module Make (P : sig
  val h_out : out_channel
  val prms : prms
end) =
struct
  open P

  let ex cmd = output_string h_out (cmd ^ "\n")

  let write_arr x =
    let filename = Filename.temp_file ~temp_dir:prms.tmp_root "ocaml_gnuplot_" "" in
    let file = Unix.(openfile filename [ O_RDWR; O_CREAT; O_TRUNC ] 0o666) in
    let x_mem =
      Unix.map_file
        file
        Bigarray.Float64
        Bigarray.c_layout
        true
        [| Mat.row_num x; Mat.col_num x |]
    in
    Bigarray.Genarray.blit x x_mem;
    Unix.close file;
    filename


  let perhaps_transpose x = if Mat.row_num x = 1 then Mat.transpose x else x

  let rec write_binary_data = function
    | F _ -> assert false
    | L xl ->
      let x =
        try Mat.concatenate ~axis:1 Array.(map perhaps_transpose (of_list xl)) with
        | _ -> failwith "plot: vectors must have the same length"
      in
      write_binary_data (A x)
    | A x ->
      let filename = write_arr x in
      let file_opt =
        Array.make Mat.(col_num x) "%double" |> Array.to_list |> String.concat ""
      in
      let file_opt = sprintf "'%s' binary format='%s'" filename file_opt in
      filename, file_opt


  let _plot plot_cmd data =
    let data =
      List.map
        (fun (x, opts) ->
          let _, f = write_binary_data x in
          sprintf "%s %s" f opts)
        data
    in
    ex (plot_cmd ^ String.concat ", " data)


  let plot = _plot "plot"
  let splot = _plot "splot"

  let heatmap mat =
    let n, m = Mat.shape mat in
    let filename, _ = write_binary_data (A mat) in
    List.iter
      ex
      [ sprintf "set xrange [%f:%f]" (-0.5) (float m -. 0.5)
      ; sprintf "set yrange [%f:%f] reverse" (-0.5) (float n -. 0.5)
      ; sprintf
          "plot '%s' binary format='%s' array=(%i,%i) w image pixels"
          filename
          "%double"
          m
          n
      ]


  let load s = ex (sprintf "load '%s'" s)

  let set (type a) ?o (prop : a property) (x : a) =
    let cmd =
      match prop with
      | Title -> sprintf "set title '%s'" x
      | Label ->
        let ax, lbl = x in
        sprintf "set %slabel '%s'" (string_of_axis ax) lbl
      | Range ->
        let ax, (a, b) = x in
        sprintf "set %srange [%f:%f]" (string_of_axis ax) a b
      | Tics ->
        let ax, ti = x in
        sprintf
          "set %stics %s"
          (string_of_axis ax)
          (match ti with
          | `list s ->
            let z =
              String.concat ", " (List.map (fun (x, la) -> sprintf "'%s' %f" la x) s)
            in
            sprintf "( %s )" z
          | `def (a0, step, a1) -> sprintf "%f, %f, %f" a0 step a1)
      | Key -> sprintf "set key %s" x
      | Palette -> sprintf "set palette %s" x
      | Format ->
        let ax, fmt = x in
        sprintf "set format %s %s" (string_of_axis ax) fmt
      | Autoscale -> sprintf "set autoscale %s" (string_of_axis x)
      | Logscale -> sprintf "set logscale %s" (string_of_axis x)
      | Text ->
        let id, lbl = x in
        sprintf "set label %i %s" id lbl
      | Border ->
        let total =
          List.fold_left
            (fun accu side ->
              accu
              +
              match side with
              | `b -> 1
              | `l -> 2
              | `t -> 4
              | `r -> 8)
            0
            x
        in
        sprintf "set border %i" total
      | Colorbox -> sprintf "set colorbox %s" x
      | Multiplot -> "set multiplot"
      | Prop -> sprintf "set %s" x
    in
    let cmd =
      match o with
      | Some o -> sprintf "%s %s" cmd o
      | None -> cmd
    in
    ex cmd


  let unset (type a) (prop : a unset_property) (x : a) =
    let cmd =
      match prop with
      | Title -> "unset title"
      | Label -> sprintf "unset %slabel" (string_of_axis x)
      | Tics -> sprintf "unset %stics" (string_of_axis x)
      | Key -> "unset key"
      | Autoscale -> sprintf "unset autoscale %s" (string_of_axis x)
      | Logscale -> sprintf "unset logscale %s" (string_of_axis x)
      | Text -> sprintf "unset label %i" x
      | Border -> "unset border"
      | Colorbox -> "unset colorbox"
      | Multiplot -> "unset multiplot"
      | Prop -> sprintf "unset %s" x
    in
    ex cmd


  let barebone () =
    unset Border ();
    List.iter (unset Tics) [ `x; `y ];
    unset Colorbox ()


  let margins =
    List.iter (function
        | `t x -> ex (sprintf "set tmargin at screen %f" x)
        | `b x -> ex (sprintf "set bmargin at screen %f" x)
        | `l x -> ex (sprintf "set lmargin at screen %f" x)
        | `r x -> ex (sprintf "set rmargin at screen %f" x))


  let multiplot
      ?(rect = (0.1, 0.1), (0.9, 0.9)) ?(spacing = 0.04, 0.04) (rows, cols) plot_fun
    =
    ex (sprintf "set multiplot layout %i,%i" rows cols);
    let (rx0, ry0), (rx1, ry1) = rect in
    let rx0, rx1 = min rx0 rx1, max rx0 rx1 in
    let ry0, ry1 = min ry0 ry1, max ry0 ry1 in
    let total_width = rx1 -. rx0 in
    let total_height = ry1 -. ry0 in
    let sp_x, sp_y = spacing in
    let h = (total_height -. (float (rows - 1) *. sp_y)) /. float rows in
    let w = (total_width -. (float (cols - 1) *. sp_x)) /. float cols in
    for k = 0 to (rows * cols) - 1 do
      let row = k / cols
      and col = k mod cols in
      let t = ry1 -. (float row *. (sp_y +. h)) in
      let b = t -. h in
      let l = rx0 +. (float col *. (sp_x +. w)) in
      let r = l +. w in
      margins [ `t t; `b b; `l l; `r r ];
      plot_fun k row col
    done;
    ex "unset multiplot"
end

let draw ?(prms = default_prms) ~output (fig : (module Figure) -> unit) =
  (* create a handle *)
  let h_out =
    let h_out = Unix.open_process_out prms.gnuplot in
    output_string h_out (sprintf "set term %s\n" (opts_of output.term));
    (match output.file with
    | Some f -> output_string h_out (sprintf "set output '%s'\n" f)
    | None -> output_string h_out "set output\n");
    output_string h_out (prms.init ^ "\n");
    flush h_out;
    h_out
  in
  (* hack to make sure that gnuplot terminates if the handle is lost *)
  Gc.finalise
    (fun _ ->
      try ignore (Unix.close_process_out h_out) with
      | _ -> ())
    h_out;
  (* create the main figure module *)
  let module F = Make (struct
    let h_out = h_out
    let prms = prms
  end)
  in
  (* draw the figure *)
  fig (module F);
  F.ex "unset multiplot";
  (* just in case -- that doesn't hurt *)
  F.ex "unset output";
  (match output.pause with
  | Some p -> F.ex p
  | None -> ());
  flush h_out;
  ignore (Unix.close_process_out h_out);
  (match output.post_action, output.file with
  | Some action, Some f -> action Fpath.(f |> v |> rem_ext |> to_string)
  | _ -> ());
  Sys.command (sprintf "rm -f %s/ocaml_gnuplot_*" prms.tmp_root) |> ignore


let interactive ?size f = draw ~output:(qt ?size ~pause:"pause mouse close" ()) f
