open Expression;;

type magnitude_t = 
  Length
  | Mass
  | Volume
  | Voltage
  | Temperature
  | Time
  | Energy
  | MagneticField
  | Angle;;

let string_of_magnitude (m: magnitude_t) = match m with
  | Length -> "length"
  | Mass -> "mass"
  | Volume -> "volume"
  | Temperature -> "temperature"
  | Voltage -> "voltage"
  | Time -> "time"
  | MagneticField -> "magnetic field"
  | Energy -> "energy"
  | Angle -> "angle";;


(* TODO: Check conversions *)
let unit_list: (string * (magnitude_t * (float -> float) * (float -> float))) list = [ (* name, (magnitude, from, to)*)
  (* LENGTH *)
  ("m", (Length, Fun.id, Fun.id));
  ("km", (Length, (fun x -> x *. 1000.), (fun x -> x /. 1000.)));
  ("cm", (Length, (fun x -> x *. 0.01), (fun x -> x /. 0.01)));
  ("ft", (Length, (fun x -> x /. 3.28084), (fun x -> x *. 3.28084)));
  ("mile", (Length, (fun x -> x /. 0.000621371), (fun x -> x *. 0.000621371)));
  ("inch", (Length, (fun x -> x /. 39.3701), (fun x -> x *. 39.3701)));
  ("yard", (Length, (fun x -> x /. 1.09361), (fun x -> x *. 1.09361))); (* 1 yard = 0.9144 meters *)
  ("nmile", (Length, (fun x -> x /. 0.000539957), (fun x -> x *. 0.000539957))); (* 1 nautical mile = 1852 meters *)

  (* MASS *)
  ("kg", (Mass, Fun.id, Fun.id));
  ("g", (Mass, (fun x -> x *. 0.001), (fun x -> x /. 0.001)));
  ("pound", (Mass, (fun x -> x /. 2.20462), (fun x -> x *. 2.20462)));
  ("oz", (Mass, (fun x -> x /. 35.274), (fun x -> x *. 35.274))); (* 1 ounce = 0.0283495 kilograms *)
  ("ton", (Mass, (fun x -> x /. 0.00110231), (fun x -> x *. 0.00110231))); (* 1 ton = 907.18474 kilograms *)

  (* VOLUME *)
  ("l", (Volume, Fun.id, Fun.id));
  ("ml", (Volume, (fun x -> x *. 0.001), (fun x -> x /. 0.001)));
  ("m3", (Volume, (fun x -> x *. 1000.), (fun x -> x /. 1000.)));
  ("gal", (Volume, (fun x -> x /. 0.264172), (fun x -> x *. 0.264172))); (* 1 gallon = 3.78541 liters *)
  ("qt", (Volume, (fun x -> x /. 1.05669), (fun x -> x *. 1.05669))); (* 1 quart = 0.946353 liters *)
  ("pt", (Volume, (fun x -> x /. 2.11338), (fun x -> x *. 2.11338))); (* 1 pint = 0.473176 liters *)
  ("fl_oz", (Volume, (fun x -> x /. 33.814), (fun x -> x *. 33.814))); (* 1 fluid ounce = 0.0295735 liters *)

  (* VOLTAGE *)
  ("v", (Voltage, Fun.id, Fun.id));
  ("dbm", (Voltage, (fun x -> (sqrt (50. /. 1000.)) *. (Float.pow 10. (x /. 20.))), (fun x -> 20. *. Float.log10 (x /. sqrt (50. /. 1000.)))));

  (* TEMPERATURE *)
  ("degk", (Temperature, Fun.id, Fun.id));
  ("degc", (Temperature, (fun x -> x +. 273.15), (fun x -> x -. 273.15)));
  ("degf", (Temperature, (fun x -> (x -. 32.) *. 5. /. 9. +. 273.15), (fun x -> (x -. 273.15) *. 9. /. 5. +. 32.)));

  (* TIME *)
  ("s", (Time, Fun.id, Fun.id));
  ("min", (Time, (fun x -> x *. 60.), (fun x -> x /. 60.)));
  ("hr", (Time, (fun x -> x *. 3600.), (fun x -> x /. 3600.)));

  (* ENERGY *)
  ("j", (Energy, Fun.id, Fun.id));
  ("kj", (Energy, (fun x -> x *. 1000.), (fun x -> x /. 1000.)));
  ("wh", (Energy, (fun x -> x *. 3600.), (fun x -> x /. 3600.)));
  ("kwh", (Energy, (fun x -> x *. 3600000.), (fun x -> x /. 3600000.)));
  ("ev", (Energy, (fun x -> x *. 1.602176634e-19), (fun x -> x /. 1.602176634e-19)));
  ("cal", (Energy, (fun x -> x *. 4.184), (fun x -> x /. 4.184))); (* 1 calorie = 4.184 joules *)
  ("kcal", (Energy, (fun x -> x *. 4184.), (fun x -> x /. 4184.))); (* 1 kilocalorie = 4184 joules *)
  ("btu", (Energy, (fun x -> x *. 1055.06), (fun x -> x /. 1055.06))); (* 1 BTU = 1055.06 joules *)

  (* MAGNETIC FIELD (B-FIELD) *)
  ("t", (MagneticField, Fun.id, Fun.id));                               (* Tesla (SI unit) *)
  ("gauss", (MagneticField, (fun x -> x /. 1e4), (fun x -> x *. 1e4))); (* Gauss: 1 Gauss = 1e-4 Tesla *)
  
  (* ANGLE *)
  ("rad", (Angle, Fun.id, Fun.id));
  ("deg", (Angle, (fun x -> x *. Float.pi /. 180.), (fun x -> x *. 180. /. Float.pi)));
];;


let units: (string, (magnitude_t * (float -> float) * (float -> float))) Hashtbl.t = Hashtbl.of_seq (List.to_seq unit_list);;

let convert (src: string) (dst: string) (operation: operation_t): float = 
  let value = Eval.eval variables operation in
  let (m1, from, _) = try Hashtbl.find units (String.lowercase_ascii src) with Not_found -> raise (Unit_nonimplemented src) in
  let (m2, _, too) = try Hashtbl.find units (String.lowercase_ascii dst) with Not_found -> raise (Unit_nonimplemented dst) in
  if m1 <> m2 then 
    raise (Incompatible_magnitudes (string_of_magnitude m1, string_of_magnitude m2)) 
  else
    from value |> too;;