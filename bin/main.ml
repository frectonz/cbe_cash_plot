module Gp = Gnuplot

module Xml = struct
  include Xml

  let filter fn xml = fold (fun acc x -> if fn x then x :: acc else acc) [] xml
end

let extract_balance body =
  let re = Str.regexp {|Your Current Balance is ETB \([0-9]+\.[0-9]+\)\.|} in
  try
    let _ = Str.search_forward re body 0 in
    Some (float_of_string (Str.matched_group 1 body))
  with Not_found -> None

let ( |> ) x f = f x

let generate_plot sms_file =
  let sms = Xml.parse_file sms_file in
  let address_is_cbe x = Xml.attrib x "address" = "CBE" in
  let balances =
    Xml.filter address_is_cbe sms
    |> List.map (fun x -> Xml.attrib x "body")
    |> List.filter_map extract_balance
    |> List.rev
  in
  print_string "Extracted balances: ";
  List.iter (fun b -> Printf.printf "%s " (string_of_float b)) balances;
  print_newline ();
  print_newline ();
  let gp = Gp.create () in
  Gp.plot_many gp
    ~output:(Gp.Output.create ~size:(1280, 720) (`Png "plot.png"))
    [
      Gp.Series.points ~title:"transaction" ~color:`Red balances;
      Gp.Series.lines ~color:`Blue balances;
    ];
  Gp.close gp;
  print_endline "Plot saved to plot.png"

let () =
  if Array.length Sys.argv <> 2 then print_endline "Provide the SMS xml file."
  else generate_plot Sys.argv.(1)
