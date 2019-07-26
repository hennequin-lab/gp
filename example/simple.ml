open Owl
open Gp

let () =
  let figure (module P : Plot) =
    let open P in
    let y0 = 1990.
    and y1 = 2018. in
    let n_bins = 1 + int_of_float (y1 -. y0) in
    let x = Mat.linspace y0 y1 n_bins in
    let y = Mat.uniform ~a:12. ~b:30. n_bins 1 in
    plot
      (L [ x; y ])
      ~style:"lp pt 7 lc 8 lw 2"
      [ barebone
      ; title "test plot"
      ; borders [ `bottom; `left ]
      ; xtics (`regular [ y0; 5.; y1 ])
      ; ytics `auto
      ; xlabel "year"
      ; ylabel "temperature"
      ]
  in
  interactive figure;
  draw ~output:(png "test1") figure;
  draw ~output:(svg "test1") figure
