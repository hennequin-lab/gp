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
    let props =
      [ barebone
      ; borders [ `bottom; `left ]
      ; xtics `auto ~o:"out nomirror"
      ; ytics `auto ~o:"out nomirror"
      ; xrange (-4., 4.)
      ; yrange (-4., 4.)
      ; (* we have a huuuuge canvas to paint on, so that we never run out of space
     * if we want to extend the figure later; typically a plot only occupies
     * a small part -- cf margins below: *)
        margins [ `left 0.2; `right 0.26; `top 0.9; `bottom 0.84 ]
      ; xlabel "variable 1"
      ; ylabel "variable 2"
      ]
    in
    P.plots
      [ item (A data) ~style:"p pt 7 lc 8 ps 0.5"; item (F "x") ~style:"l lc 7 dt 2" ]
      props
  in
  let tex =
    ""
    (* you can pass on tex code to be inserted in the tikz picture if you want *)
  in
  draw ~output:(tikz ~grid:false ~font:cmbright ~tex "lab_figure") figure
