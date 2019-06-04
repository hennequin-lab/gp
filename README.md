# Basic usage

Please see the [API documentation](https://hennequin-lab.github.io/docs/gp/Gp).

```ocaml
open Gp

(* define your figure generically *)
let figure (module F: Figure) =
  let x = Owl.Mat.gaussian 10 2 in
  F.plot (A x) ~style:"p pt 7 lc 8"

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
  let figure (module F : Figure) =
    let open F in (* recommended *)
    set Title "Test plot";
    set Border [ `b; `l ];
    set Label (`x, "year");
    set Label (`y, "temperature");
    let y0 = 1990.
    and y1 = 2018. in
    set Tics (`x, `def (y0, 5., y1));
    set Tics (`y, `def (10., 5., 30.));
    set Range (`x, (y0, y1));
    unset Key ();
    set Autoscale `y;
    let n_bins = int_of_float (y1 -. y0) in
    let x = Mat.linspace y0 y1 n_bins in
    let y = Mat.uniform ~a:12. ~b:30. n_bins 1 in
    plot (L [ x; y ]) ~style:"lp pt 7 lc 8 lw 2"
  in
  interactive figure
```
