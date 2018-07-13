# Basic usage

```ocaml
open Gp

(* define your figure generically *)
let my_plot (module F: Figure) =
  F.ex "plot sin(x) w l lc 8 lw 3";
  F.draw ()

(* then paint your figure with whatever output format you'd like *)
let _ = my_plot (quick ()) (* QT interactive terminal *)
let _ = my_plot (figure ~to_file:"test" (module PNG)) (* output to "test.png" *)
let _ = my_plot (figure ~to_file:"test" (module SVG)) (* output to "test.svg" *)
```


