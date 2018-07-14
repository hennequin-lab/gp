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

module SVG : Output = struct
  let term = "svg"
  let term_opts = [ "font 'Helvetica,10'"; "size 600,400" ]
  let file_ext = ".svg"
  let post_action = None
end

module PNG : Output = struct
  let term = "pngcairo"
  let term_opts = [ "enhanced"; "color"; "transparent"; "crop"; "font 'Helvetica,10'"; "size 600,400" ]
  let file_ext = ".png"
  let post_action = None
end

module QT : Output = struct
  let term = "qt"
  let term_opts = [ "enhanced"; "persist"; "raise"; "font 'Helvetica,10'"; "size 600,400" ]
  let file_ext = "" (* irrelevant *)
  let post_action = None
end

module LaTeX : Output = struct
  let term = "cairolatex"
  let term_opts = [ "pdf"; "standalone"; "size 100cm, 100cm"; "dl 0.5";
                    "header '\\usepackage[scaled=1]{helvet}\\usepackage{sfmath,xcolor}\
                     \\renewcommand{\\familydefault}{\\sfdefault}'" ]
  let file_ext = ".tex"
  let post_action = Some (fun root -> ignore (Sys.command (sprintf "gp2pdf %s.tex" root)))
end


(* ----------------------------------------------------------------------------
   --    Figure parameters                                                   --
   ---------------------------------------------------------------------------- *)

module type Parameters = sig
  val gnuplot: string
  val init: string
  val to_file: string option
end

let default_init = "set key noautotitle; \
                    set border 3; \
                    set tics out nomirror"



(* ----------------------------------------------------------------------------
   --    Main gnuplot types and module                                       --
   ---------------------------------------------------------------------------- *)

(* properties to be set / unset *)

type axis = [ `x | `x2 | `y | `y2 | `z | `cb ]

let string_of_axis = function 
  | `x -> "x" 
  | `x2 -> "x2" 
  | `y -> "y" 
  | `y2 -> "y2" 
  | `z -> "z" 
  | `cb -> "cb"

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
  val multiplot: ?rect:((float*float)*(float*float)) -> ?spacing:(float * float) 
    ->  int * int -> (int -> int -> int -> unit) -> unit
end



(* main module *)
module New_figure (O: Output) (P: Parameters) : Figure = struct


  (* create a handle *)
  let h =
    let h = Unix.open_process_out P.gnuplot in
    List.iter (fun opt ->
        output_string h (sprintf "set term %s %s\n" O.term opt)
      ) O.term_opts;
    begin match P.to_file with 
      | Some r -> output_string h (sprintf "set output '%s%s'\n" r O.file_ext);
      | None -> ()
    end;
    output_string h (P.init ^ "\n");
    h

  (* hack to make sure that gnuplot terminates if the handle is lost *)
  let a = ref 0
  let _ = Gc.finalise (fun _ -> try ignore (Unix.close_process_out h) with _ -> ()) a

  let ex cmd = output_string h (cmd^"\n")
  let flush () = flush h
  let close () = ignore (Unix.close_process_out h)
  let draw () = 
    ex "unset multiplot"; (* just in case -- that doesn't hurt *)
    ex "unset output";
    flush ();
    close ();
    match O.post_action, P.to_file with
    | Some f, Some r -> f r
    | _ -> ()

  let end_signal () = fprintf h "e\n%!"

  let __send_columns m =
    let cols = Array.length m in
    let rows = Array.fold_left max (-1) (Array.map Array.length m) in
    for i=0 to rows-1 do
      for j=0 to cols-1 do
        let mj = m.(j) in
        if i<Array.length mj
        then fprintf h "%f " mj.(i)
        else fprintf h "- ";
      done; fprintf h "\n%!";
    done;
    end_signal ()

  let send_columns m =
    __send_columns (Array.map (fun x ->
        let a, b = Mat.shape x in
        Mat.to_array (if a < b then (assert (a=1); Mat.transpose x) else (assert (b=1); x))
      ) m)


  (* for some reason, gnuplot wants a double "e" at the end
     of the stream for matrix data given to [splot()] ... *)

  let send_matrix m =
    send_columns (Mat.to_cols m);
    end_signal ()

  let plot data =
    let cmds = Array.map (fun (_,opts) -> sprintf "'-' %s" opts) data |> Array.to_list in
    ex ("plot " ^ String.concat ", " cmds);
    Array.iter (fun (d,_) -> send_columns (Array.of_list d)) data

  let splot data =
    let mat, opts = data in
    ex "set pm3d map";
    let n, m = Mat.shape mat in
    List.iter ex [
      sprintf "set xrange [%f:%f]" (-0.5) (float n -. 0.5);
      sprintf "set yrange [%f:%f] reverse" (-0.5) (float m -. 0.5);
      sprintf "splot '-' %s" opts
    ];
    send_matrix mat

  let image mat =
    let n, m = Mat.shape mat in
    List.iter ex [
      sprintf "set xrange [%f:%f]" (-0.5) (float m -. 0.5);
      sprintf "set yrange [%f:%f] reverse" (-0.5) (float n -. 0.5);
      "plot '-' mat w image"];
    send_matrix mat

  let load s = ex (sprintf "load '%s'" s)

  let set (type a) ?o (prop: a property) (x: a) = 
    let cmd = match prop with
      | Title -> sprintf "set title '%s'" x
      | Label -> 
        let ax, lbl = x in
        sprintf "set %slabel '%s'" (string_of_axis ax) lbl
      | Range ->
        let ax, (a, b) = x in
        sprintf "set %srange [%f:%f]" (string_of_axis ax) a b
      | Tics -> 
        let ax, ti = x in
        sprintf "set %stics %s" (string_of_axis ax)
          (match ti with
           | `list s -> 
             let z = String.concat ", " (List.map (fun (x,la) -> sprintf "'%s' %f" la x) s) in
             sprintf "( %s )" z
           | `def (a0,step,a1) -> sprintf "%f, %f, %f" a0 step a1)
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
        let total = List.fold_left (fun accu side ->
            accu + match side with `b -> 1 | `l -> 2 | `t -> 4 | `r -> 8) 0 x in
        sprintf "set border %i" total
      | Colorbox -> sprintf "set colorbox %s" x
      | Multiplot -> "set multiplot"
      | Prop -> sprintf "set %s" x

    in
    let cmd = match o with Some o -> sprintf "%s %s" cmd o | None -> cmd in
    ex cmd

  let unset (type a) (prop: a unset_property) (x: a) = 
    let cmd = match prop with
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


  let margins = List.iter (function
      | `t x -> ex (sprintf "set tmargin at screen %f" x)
      | `b x -> ex (sprintf "set bmargin at screen %f" x)
      | `l x -> ex (sprintf "set lmargin at screen %f" x)
      | `r x -> ex (sprintf "set rmargin at screen %f" x))

  let multiplot ?(rect=((0.1, 0.1), (0.9,0.9))) ?(spacing=(0.04,0.04)) (rows, cols) plot_fun =
    ex (sprintf "set multiplot layout %i,%i" rows cols);
    let (rx0, ry0), (rx1, ry1) = rect in
    let rx0, rx1 = min rx0 rx1, max rx0 rx1 in
    let ry0, ry1 = min ry0 ry1, max ry0 ry1 in
    let total_width = rx1 -. rx0 in
    let total_height = ry1 -. ry0 in
    let sp_x, sp_y = spacing in
    let h = (total_height -. float (rows-1) *. sp_y) /. float rows in
    let w = (total_width -. float (cols-1) *. sp_x) /. float cols in
    for k=0 to rows*cols - 1 do 
      let row = k/cols and col = k mod cols in
      let t = ry1 -. float row *. (sp_y +. h) in
      let b = t -. h in
      let l = rx0 +. float col *. (sp_x +. w) in
      let r = l +. w in
      margins [`t t; `b b; `l l; `r r];
      plot_fun k row col;
    done;
    ex "unset multiplot"

end


let figure ?(gnuplot="gnuplot") ?(init=default_init) ?to_file (module O: Output) = 
  let module P = struct
    let gnuplot = gnuplot
    let init = init
    let to_file = to_file
  end in
  let module F = New_figure (O) (P) in
  (module F: Figure)

let quick () = figure (module QT)



