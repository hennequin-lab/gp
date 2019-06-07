open Gp

(** How we make our figures for scientific journals *)

let data =
  let c = 0.8 in
  let open Owl in
  let n = 100 in
  let common = Mat.gaussian n 1 in
  Mat.((c $* common) + (Maths.(sqrt (1. -. (c *. c))) $* gaussian n 2))


let () =
  let figure (module P : Plot) =
    P.barebone ();
    P.set Border [ `b; `l ];
    P.set Tics (`x, `auto) ~o:"out nomirror";
    P.set Tics (`y, `auto) ~o:"out nomirror";
    P.set Range (`x, (-4., 4.));
    P.set Range (`y, (-4., 4.));
    (* we have a huuuuge canvas to paint on, so that we never run out of space
     * if we want to extend the figure later; typically a plot only occupies
     * a small part -- cf margins below: *)
    P.margins [ `l 0.2; `r 0.26; `t 0.9; `b 0.84 ];
    P.set Label (`x, "variable 1");
    P.set Label (`y, "variable 2");
    P.plots
      [ item (A data) ~style:"p pt 7 lc 8 ps 0.5"; item (F "x") ~style:"l lc 7 dt 2" ]
  in
  let tex =
    ""
    (* you can pass on tex code to be inserted in the tikz picture if you want *)
  in
  draw ~output:(tikz ~grid:false ~font:cmbright ~tex "lab_figure") figure
