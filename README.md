# Basic usage

Please see the [API documentation](https://hennequin-lab.github.io/docs/gp/Gp).

```ocaml
open Gp

(* define your figure generically *)
let figure (module P: Plot) =
  let x = Owl.Mat.gaussian 10 2 in
  P.plot (A x) ~style:"p pt 7 lc 8" default_props

(* then paint your figure with an output format of your choice *)
let () =
  interactive figure; (* QT interactive terminal *)
  draw ~output:(png "test") figure; (* output to "test.png" *)
  draw ~output:(svg "test") figure; (* output to "test.svg" *)
```

# More advanced usage: example

```ocaml
open Owl
open Gp

let () =
  let y0 = 1990.
  and y1 = 2018. in
  let n_bins = int_of_float (y1 -. y0) in
  let x = Mat.linspace y0 y1 n_bins in
  let y = Mat.uniform ~a:12. ~b:30. n_bins 1 in
  let figure (module P : Plot) =
    let props =
      [ title "Test plot"
      ; borders [ `bottom; `left ]
      ; xlabel "year"
      ; ylabel "temperature"
      ; xtics (`regular [ y0; 5.; y1 ])
      ; ytics `auto
      ; xrange (y0, y1)
      ; unset "key"
      ; set "autoscale y"
      ]
    in
    P.plot (L [ x; y ]) ~style:"lp pt 7 lc 8 lw 2" props
  in
  interactive figure
```
