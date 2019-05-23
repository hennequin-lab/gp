open Printf
open Owl

let _ = Printexc.record_backtrace true

(* ----------------------------------------------------------------------------
   --    Output terminals                                                    --
   ---------------------------------------------------------------------------- *)

type term = {term: string; font: string option; size: (int * int) option; other: string option}

let opts_of z =
  let s = z.term in
  let s = match z.other with Some x -> sprintf "%s %s" s x | None -> s in
  let s = match z.font with Some x -> sprintf "%s font '%s'" s x | None -> s in
  let s = match z.size with Some (x, y) -> sprintf "%s size %i,%i" s x y | None -> s in
  s

module type Output = sig
  val term : term
  val file_ext : string
  val post_action : (string -> unit) option

  (* possibly do something with the root filename after "draw" *)
end

module SVG : Output = struct
  let term = {term= "svg"; font= Some "Helvetica,12"; size= Some (600, 400); other= None}
  let file_ext = ".svg"
  let post_action = None
end

module PNG : Output = struct
  let term =
    { term= "pngcairo";
      other= Some "enhanced color notransparent crop";
      font= Some "Helvetica,10";
      size= Some (600, 400) }

  let file_ext = ".png"
  let post_action = None
end

module QT : Output = struct
  let term =
    { term= "qt";
      font= Some "Helvetica,10";
      size= Some (600, 400);
      other= Some "enhanced persist raise" }

  let file_ext = ""

  (* irrelevant *)
  let post_action = None
end

module LaTeX : Output = struct
  let term =
    { term= "cairolatex";
      size= None;
      font= None;
      other=
        Some
          "pdf standalone size 100cm, 100cm dl 0.5 header \
           '\\usepackage[scaled=1]{helvet}\\usepackage{sfmath,xcolor}\\renewcommand{\\familydefault}{\\sfdefault}'"
    }

  let file_ext = ".tex"
  let post_action = Some (fun root -> ignore (Sys.command (sprintf "pdflatex %s.tex" root)))
end

(* ----------------------------------------------------------------------------
   --    Figure parameters                                                   --
   ---------------------------------------------------------------------------- *)

module type Parameters = sig
  val gnuplot : string
  val init : string
  val to_file : string option
end

let default_init = "set key noautotitle; set border 3; set tics out nomirror"

(* ----------------------------------------------------------------------------
   --    Main gnuplot types and module                                       --
   ---------------------------------------------------------------------------- *)

(* properties to be set / unset *)

type axis = [`x | `x2 | `y | `y2 | `z | `cb]

let string_of_axis = function
  | `x ->
      "x"
  | `x2 ->
      "x2"
  | `y ->
      "y"
  | `y2 ->
      "y2"
  | `z ->
      "z"
  | `cb ->
      "cb"

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

type data = A of Mat.mat | L of Mat.mat list | F of (float -> float)

(** Contains all the commands you need to draw your figure *)
module type Figure = sig
  val h_out : out_channel
  val ex : string -> unit
  val draw : unit -> unit
  val plot : (data * string) list -> unit
  val splot : (data * string) list -> unit
  val heatmap : Mat.mat -> unit
  val load : string -> unit
  val set : ?o:string -> 'a property -> 'a -> unit
  val unset : 'a unset_property -> 'a -> unit

  val barebone : unit -> unit
  (** Trim the plot to the bare minimum: no axes, no labels, no tics, nothing but your lovely plot *)

  val margins : [`t of float | `b of float | `l of float | `r of float] list -> unit
  (** Set the plot margins in screen coordinates; (0,0) = bottom left, (1,1) = top right *)

  val multiplot :
    ?rect:(float * float) * (float * float) ->
    ?spacing:float * float ->
    int * int ->
    (int -> int -> int -> unit) ->
    unit
end

(* main module *)
module New_figure (O : Output) (P : Parameters) : Figure = struct
  (* create a handle *)
  let h_out =
    let h_out = Unix.open_process_out P.gnuplot in
    output_string h_out (sprintf "set term %s\n" (opts_of O.term)) ;
    ( match P.to_file with
    | Some r ->
        output_string h_out (sprintf "set output '%s%s'\n" r O.file_ext)
    | None ->
        output_string h_out "set output\n" ) ;
    output_string h_out (P.init ^ "\n") ;
    flush h_out ;
    h_out

  (* hack to make sure that gnuplot terminates if the handle is lost *)
  let a = ref 0
  let _ = Gc.finalise (fun _ -> try ignore (Unix.close_process_out h_out) with _ -> ()) a
  let ex cmd = output_string h_out (cmd ^ "\n")
  let flush () = flush h_out

  let close () =
    Pervasives.flush h_out ;
    ignore (Unix.close_process_out h_out)

  let draw () =
    ex "unset multiplot" ;
    (* just in case -- that doesn't hurt *)
    ex "unset output" ;
    flush () ;
    close () ;
    (match (O.post_action, P.to_file) with Some f, Some r -> f r | _ -> ()) ;
    Sys.command "rm -f /dev/shm/ocaml_gnuplot_*" |> ignore

  let write_arr x =
    let filename = Filename.temp_file ~temp_dir:"/dev/shm" "ocaml_gnuplot_" "" in
    let file = Unix.(openfile filename [O_RDWR; O_CREAT; O_TRUNC] 0o666) in
    let x_mem =
      Unix.map_file file Bigarray.Float64 Bigarray.c_layout true [|Mat.row_num x; Mat.col_num x|]
    in
    Bigarray.Genarray.blit x x_mem ; Unix.close file ; filename

  let perhaps_transpose x = if Mat.row_num x = 1 then Mat.transpose x else x

  let rec write_binary_data = function
    | F _ ->
        assert false
    | L xl ->
        let x =
          try Mat.concatenate ~axis:1 Array.(map perhaps_transpose (of_list xl))
          with _ -> failwith "plot: vectors must have the same length"
        in
        write_binary_data (A x)
    | A x ->
        let filename = write_arr x in
        let file_opt = Array.make Mat.(col_num x) "%double" |> Array.to_list |> String.concat "" in
        let file_opt = sprintf "'%s' binary format='%s'" filename file_opt in
        (filename, file_opt)

  let _plot plot_cmd data =
    let data =
      List.map
        (fun (x, opts) ->
          let _, f = write_binary_data x in
          sprintf "%s %s" f opts )
        data
    in
    ex (plot_cmd ^ String.concat ", " data)

  let plot = _plot "plot"
  let splot = _plot "splot"

  let heatmap mat =
    let n, m = Mat.shape mat in
    let filename, _ = write_binary_data (A mat) in
    List.iter ex
      [ sprintf "set xrange [%f:%f]" (-0.5) (float m -. 0.5);
        sprintf "set yrange [%f:%f] reverse" (-0.5) (float n -. 0.5);
        sprintf "plot '%s' binary format='%s' array=(%i,%i) w image pixels" filename "%double" m n
      ]

  let load s = ex (sprintf "load '%s'" s)

  let set (type a) ?o (prop : a property) (x : a) =
    let cmd =
      match prop with
      | Title ->
          sprintf "set title '%s'" x
      | Label ->
          let ax, lbl = x in
          sprintf "set %slabel '%s'" (string_of_axis ax) lbl
      | Range ->
          let ax, (a, b) = x in
          sprintf "set %srange [%f:%f]" (string_of_axis ax) a b
      | Tics ->
          let ax, ti = x in
          sprintf "set %stics %s" (string_of_axis ax)
            ( match ti with
            | `list s ->
                let z = String.concat ", " (List.map (fun (x, la) -> sprintf "'%s' %f" la x) s) in
                sprintf "( %s )" z
            | `def (a0, step, a1) ->
                sprintf "%f, %f, %f" a0 step a1 )
      | Key ->
          sprintf "set key %s" x
      | Palette ->
          sprintf "set palette %s" x
      | Format ->
          let ax, fmt = x in
          sprintf "set format %s %s" (string_of_axis ax) fmt
      | Autoscale ->
          sprintf "set autoscale %s" (string_of_axis x)
      | Logscale ->
          sprintf "set logscale %s" (string_of_axis x)
      | Text ->
          let id, lbl = x in
          sprintf "set label %i %s" id lbl
      | Border ->
          let total =
            List.fold_left
              (fun accu side -> accu + match side with `b -> 1 | `l -> 2 | `t -> 4 | `r -> 8)
              0 x
          in
          sprintf "set border %i" total
      | Colorbox ->
          sprintf "set colorbox %s" x
      | Multiplot ->
          "set multiplot"
      | Prop ->
          sprintf "set %s" x
    in
    let cmd = match o with Some o -> sprintf "%s %s" cmd o | None -> cmd in
    ex cmd

  let unset (type a) (prop : a unset_property) (x : a) =
    let cmd =
      match prop with
      | Title ->
          "unset title"
      | Label ->
          sprintf "unset %slabel" (string_of_axis x)
      | Tics ->
          sprintf "unset %stics" (string_of_axis x)
      | Key ->
          "unset key"
      | Autoscale ->
          sprintf "unset autoscale %s" (string_of_axis x)
      | Logscale ->
          sprintf "unset logscale %s" (string_of_axis x)
      | Text ->
          sprintf "unset label %i" x
      | Border ->
          "unset border"
      | Colorbox ->
          "unset colorbox"
      | Multiplot ->
          "unset multiplot"
      | Prop ->
          sprintf "unset %s" x
    in
    ex cmd

  let barebone () =
    unset Border () ;
    List.iter (unset Tics) [`x; `y] ;
    unset Colorbox ()

  let margins =
    List.iter (function
      | `t x ->
          ex (sprintf "set tmargin at screen %f" x)
      | `b x ->
          ex (sprintf "set bmargin at screen %f" x)
      | `l x ->
          ex (sprintf "set lmargin at screen %f" x)
      | `r x ->
          ex (sprintf "set rmargin at screen %f" x) )

  let multiplot ?(rect = ((0.1, 0.1), (0.9, 0.9))) ?(spacing = (0.04, 0.04)) (rows, cols) plot_fun
      =
    ex (sprintf "set multiplot layout %i,%i" rows cols) ;
    let (rx0, ry0), (rx1, ry1) = rect in
    let rx0, rx1 = (min rx0 rx1, max rx0 rx1) in
    let ry0, ry1 = (min ry0 ry1, max ry0 ry1) in
    let total_width = rx1 -. rx0 in
    let total_height = ry1 -. ry0 in
    let sp_x, sp_y = spacing in
    let h = (total_height -. (float (rows - 1) *. sp_y)) /. float rows in
    let w = (total_width -. (float (cols - 1) *. sp_x)) /. float cols in
    for k = 0 to (rows * cols) - 1 do
      let row = k / cols and col = k mod cols in
      let t = ry1 -. (float row *. (sp_y +. h)) in
      let b = t -. h in
      let l = rx0 +. (float col *. (sp_x +. w)) in
      let r = l +. w in
      margins [`t t; `b b; `l l; `r r] ;
      plot_fun k row col
    done ;
    ex "unset multiplot"
end

let plot (module Canvas : Figure) fig =
  fig (module Canvas : Figure) ;
  Canvas.draw ()

let figure ?(gnuplot = "gnuplot") ?(init = default_init) ?to_file (module O : Output) =
  let module P = struct
    let gnuplot = gnuplot
    let init = init
    let to_file = to_file
  end in
  let module F = New_figure (O) (P) in
  (module F : Figure)

let quick ?(size = (600, 400)) (f : (module Figure) -> unit) =
  let module O : Output = struct
    let term =
      {term= "qt"; font= Some "Helvetica,10"; size= Some size; other= Some "enhanced persist raise"}

    let file_ext = ""

    (* irrelevant *)
    let post_action = None
  end in
  let fig = figure (module O) in
  let module F = (val fig : Figure) in
  f (module F) ;
  F.draw ()
