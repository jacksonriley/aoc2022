  let read_input_from_stdin = 
  let ch = stdin in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let explode_string s = List.init (String.length s) (String.get s)
let flip2 f x y = f y x
