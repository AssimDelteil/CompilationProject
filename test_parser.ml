open File
let lexbuf = Lexing.from_channel stdin

let _ =
  while true do
    let f = Parser.s (Lexer.decoupe) lexbuf in
    File.aff_file f;
    print_newline ()
  done