open Printf
open Owl

let default_tmp_root =
  match Unix.stat "/dev/shm" with
  | {st_kind = S_DIR; _} -> "/dev/shm"
  | _ -> "/tmp"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> "/tmp"

type prms =
  { tmp_root : string
  ; gnuplot : string
  ; init : string
  }

let prms
    ?(tmp_root = default_tmp_root)
    ?(gnuplot = "gnuplot")
    ?(init = "set key noautotitle; set border 3; set tics out nomirror")
    ()
  =
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


let remove_ext ext name =
  let name = Fpath.(v name) in
  (if Fpath.has_ext ext name then Fpath.rem_ext name else name) |> Fpath.to_string


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
    ?(other_term_opts = "enhanced color transparent crop")
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
    ?(other_term_opts = "enhanced raise")
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
  ; post_action =
      Some (fun root -> ignore (Sys.command (sprintf "pdflatex %s.tex" root)))
  }


type tikz_font = string

let cmbright = Tikz.font_cmbright
let helvetica = Tikz.font_helvetica

let tikz ?(grid = false) ?(crop = true) ?(font = cmbright) ?(tex = "") file_name =
  let tex_code = tex in
  let file_name = ensure_ext "tex" file_name in
  let tmp = Filename.temp_file ~temp_dir:default_tmp_root "ocaml_gnuplot_" "" in
  let post_action =
    Some
      (fun _ ->
        (* populate file_name.tex *)
        let tex = Tikz.preamble_template ~grid font in
        let tex = tex ^ Tikz.include_fig tmp in
        let tex = tex ^ "\n" ^ tex_code ^ "\n" ^ Tikz.end_template in
        let file = open_out file_name in
        output_string file tex;
        close_out file;
        let r = remove_ext "tex" file_name in
        assert (0 = Sys.command Printf.(sprintf "pdflatex %s.tex" r));
        if crop then assert (0 = Sys.command Printf.(sprintf "pdfcrop %s.pdf %s.pdf" r r));
        assert (0 = Sys.command Printf.(sprintf "rm -f %s.aux %s.log* %s.tex" r r r));
        assert (0 = Sys.command Printf.(sprintf "rm -f %s*" tmp)))
  in
  { term =
      { term = "cairolatex"
      ; size = None
      ; font = None
      ; other = Some "pdf input size 100cm, 100cm dl 0.5"
      }
  ; file = Some (ensure_ext "tex" tmp)
  ; pause = None
  ; post_action
  }


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

type property = string

let set = sprintf "set %s"
let unset = sprintf "unset %s"

let barebone =
  "unset border; unset tics; unset label; unset xlabel; unset ylabel; unset zlabel; \
   unset cblabel; unset colorbox"


let with_opts ?o s =
  match o with
  | None -> s
  | Some o -> sprintf "%s %s" s o


let title ?o title = with_opts ?o (sprintf "set title '%s'" title)

let margins x =
  x
  |> List.map (function
         | `left x -> sprintf "set lmargin at screen %f" x
         | `right x -> sprintf "set rmargin at screen %f" x
         | `top x -> sprintf "set tmargin at screen %f" x
         | `bottom x -> sprintf "set bmargin at screen %f" x)
  |> String.concat ";"


let offsets x =
  let to_string = function
    | `graph z -> sprintf "graph %f" z
    | `first z -> sprintf "%f" z
  in
  let find f = List.fold_left f "0" x in
  let left =
    find (fun accu ->
      function
      | `left z -> to_string z
      | _ -> accu)
  in
  let right =
    find (fun accu ->
      function
      | `right z -> to_string z
      | _ -> accu)
  in
  let top =
    find (fun accu ->
      function
      | `top z -> to_string z
      | _ -> accu)
  in
  let bottom =
    find (fun accu ->
      function
      | `bottom z -> to_string z
      | _ -> accu)
  in
  String.concat ", " [ left; right; top; bottom ] |> sprintf "set offsets %s"


let borders ?o x =
  let total =
    List.fold_left
      (fun accu (side : side) ->
        accu
        +
        match side with
        | `bottom -> 1
        | `left -> 2
        | `top -> 4
        | `right -> 8)
      0
      x
  in
  with_opts ?o (sprintf "set border %i" total)


let tics = sprintf "set tics %s"

let _tics label ?(o = "out nomirror") x =
  let x =
    match x with
    | `auto -> "autofreq"
    | `regular l -> l |> List.map string_of_float |> String.concat ", "
    | `manual l ->
      let z =
        l |> List.map (fun (x, la) -> sprintf "'%s' %f" la x) |> String.concat ", "
      in
      sprintf "( %s )" z
  in
  with_opts ~o (sprintf "set %stics %s" label x)


let xtics = _tics "x"
let ytics = _tics "y"
let ztics = _tics "z"
let cbtics = _tics "cb"
let x2tics = _tics "x2"
let y2tics = _tics "y2"
let _label c ?o s = with_opts ?o (sprintf "set %slabel '%s'" c s)
let xlabel = _label "x"
let ylabel = _label "y"
let zlabel = _label "z"
let cblabel = _label "cb"
let x2label = _label "x2"
let y2label = _label "y2"
let _range c ?o (a, b) = with_opts ?o (sprintf "set %srange [%f:%f]" c a b)
let xrange = _range "x"
let yrange = _range "y"
let zrange = _range "z"
let cbrange = _range "cb"
let x2range = _range "x2"
let y2range = _range "y2"
let default_props = [ barebone; borders [ `left; `bottom ]; xtics `auto; ytics `auto ]

type data =
  | A of Mat.mat
  | L of Mat.mat list
  | F of ((float -> float) * Mat.mat)
  | S of string

let perhaps_transpose =
  let f x = if Mat.row_num x = 1 then Mat.transpose x else x in
  function
  | A x -> A (f x)
  | L x -> L (List.map f x)
  | F f -> F f
  | S s -> S s


type item = data * string

let item ?using ?axes ?legend ?style data =
  let data = perhaps_transpose data in
  let using =
    match using with
    | Some u -> "using " ^ u
    | None ->
      (match data with
      | A x -> if Mat.col_num x = 1 then "using 0:1" else "using 1:2"
      | L x -> if List.length x = 1 then "using 0:1" else "using 1:2"
      | F _ -> "using 1:2"
      | S _ -> "")
  in
  let axes =
    match axes with
    | Some a -> "axes " ^ a
    | None -> ""
  in
  let legend =
    match legend with
    | None -> ""
    | Some t -> sprintf "title '%s'" t
  in
  let style =
    match style with
    | Some s -> "with " ^ s
    | None -> "with l lc 8"
  in
  let opts = String.concat " " [ using; axes; legend; style ] in
  data, opts


(** Contains all the commands you need to draw your figure *)
module type Plot = sig
  val ex : string -> unit

  val plot
    :  ?using:string
    -> ?axes:string
    -> ?legend:string
    -> ?style:string
    -> data
    -> property list
    -> unit

  val plots : item list -> property list -> unit

  val splot
    :  ?using:string
    -> ?axes:string
    -> ?legend:string
    -> ?style:string
    -> data
    -> property list
    -> unit

  val splots : item list -> property list -> unit
  val heatmap : ?style:string -> Mat.mat -> property list -> unit
  val load : string -> unit

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


  let rec write_binary_data = function
    | F (f, x) ->
      let x =
        if Mat.col_num x = 1
        then x
        else if Mat.row_num x = 1
        then Mat.transpose x
        else failwith "x must be a vector"
      in
      let y = Mat.map f x in
      let data = Mat.concat_horizontal x y in
      let filename = write_arr data in
      let file_opt = "%" ^ string_of_int Mat.(col_num data) ^ "double" in
      let file_opt = sprintf "'%s' binary format='%s'" filename file_opt in
      filename, file_opt
    | S s -> "", s
    | L xl ->
      let x =
        try Mat.concatenate ~axis:1 Array.(of_list xl) with
        | _ -> failwith "plot: vectors must have the same length"
      in
      write_binary_data (A x)
    | A x ->
      let filename = write_arr x in
      let file_opt = "%" ^ string_of_int Mat.(col_num x) ^ "double" in
      let file_opt = sprintf "'%s' binary format='%s'" filename file_opt in
      filename, file_opt


  let set_properties = List.iter ex

  let _plot plot_cmd data prop =
    set_properties prop;
    let data =
      List.map
        (fun (x, opts) ->
          let _, f = write_binary_data x in
          sprintf "%s %s" f opts)
        data
    in
    ex (plot_cmd ^ " " ^ String.concat ", " data)


  let plots = _plot "plot"
  let splots = _plot "splot"

  let plot ?using ?axes ?legend ?style data prop =
    plots [ item ?using ?axes ?legend ?style data ] prop


  let splot ?using ?axes ?legend ?style data prop =
    splots [ item ?using ?axes ?legend ?style data ] prop


  let heatmap ?(style = "image") mat prop =
    set_properties prop;
    let n, m = Mat.shape mat in
    let filename, _ = write_binary_data (A mat) in
    List.iter
      ex
      [ sprintf "set xrange [%f:%f]" (-0.5) (float m -. 0.5)
      ; sprintf "set yrange [%f:%f] reverse" (-0.5) (float n -. 0.5)
      ; sprintf
          "plot '%s' binary format='%s' array=(%i,%i) w %s"
          filename
          "%double"
          m
          n
          style
      ]


  let load s = ex (sprintf "load '%s'" s)

  let multiplot
      ?(rect = (0.1, 0.1), (0.9, 0.9))
      ?(spacing = 0.04, 0.04)
      (rows, cols)
      plot_fun
    =
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
      ex (sprintf "set tmargin at screen %f" t);
      ex (sprintf "set bmargin at screen %f" b);
      ex (sprintf "set lmargin at screen %f" l);
      ex (sprintf "set rmargin at screen %f" r);
      plot_fun k row col
    done
end

let draw ?(prms = default_prms) ~output (fig : (module Plot) -> unit) =
  (* create a handle *)
  let h_out =
    let h_out = Unix.open_process_out prms.gnuplot in
    output_string h_out (sprintf "set term %s\n" (opts_of output.term));
    (match output.file with
    | Some f -> output_string h_out (sprintf "set output '%s'\n" f)
    | None -> output_string h_out "set output\n");
    output_string h_out (prms.init ^ "\n");
    output_string h_out "set multiplot\n";
    flush h_out;
    h_out
  in
  (* create the main figure module *)
  let module P =
    Make (struct
      let h_out = h_out
      let prms = prms
    end)
  in
  (* draw the figure *)
  fig (module P);
  P.ex "unset multiplot";
  P.ex "unset output";
  (match output.pause with
  | Some p -> P.ex p
  | None -> ());
  flush h_out;
  ignore (Unix.close_process_out h_out);
  (match output.post_action, output.file with
  | Some action, Some f -> action Fpath.(f |> v |> rem_ext |> to_string)
  | _ -> ());
  Sys.command (sprintf "rm -f %s/ocaml_gnuplot_*" prms.tmp_root) |> ignore


let interactive ?(interactive = true) ?size f =
  let pause = if interactive then Some "pause mouse close" else None in
  draw ~output:(qt ?size ?pause ()) f
