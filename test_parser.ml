open File
let lexbuf = Lexing.from_channel stdin

let _ =
    let f = Parser.s (Lexer.decoupe) lexbuf in
    File.aff_file f;
    print_string "\n";
    File.print_consts f