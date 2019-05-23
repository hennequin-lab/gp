# Basic usage

```ocaml
open Gp

(* define your figure generically *)
let my_plot (module F: Figure) =
  F.ex "plot sin(x) w l lc 8 lw 3"

(* then paint your figure with whatever output format you'd like *)
let _ = quick my_plot (* QT interactive terminal *)
let _ = my_plot (figure ~to_file:"test" (module PNG)) (* output to "test.png" *)
let _ = my_plot (figure ~to_file:"test" (module SVG)) (* output to "test.svg" *)
```


# More advanced usage: example

```ocaml
open Owl
open Gp

let my_plot (module F: Figure) =
  let open F in
  set Title "Test plot";
  set Border [`b; `l];
  set Label (`x, "year");
  set Label (`y, "temperature");
  let y0 = 1990. and y1 = 2018. in
  set Tics (`x, `def (y0, 5., y1));
  set Tics (`y, `def (10., 5., 30.)); 
  set Range (`x, (y0, y1));
  unset Key ();
  set Autoscale `y;
  let n_bins = (int_of_float (y1 -. y0)) in
  let x = Mat.linspace y0 y1 n_bins in
  let y = Mat.uniform ~a:12. ~b:30. n_bins 1 in
  plot [| [x;y], "w lp pt 7 lc 8 lw 2" |]

let _ = my_plot |> quick (* opens a QT window *)
let _ = my_plot |> plot (figure ~to_file:"simple_example" (module PNG))
let _ = my_plot |> plot (figure ~to_file:"simple_example" (module SVG))
```
