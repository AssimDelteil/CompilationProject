open File
let lexbuf = Lexing.from_channel stdin

let _ =
    let f = Parser.s (Lexer.decoupe) lexbuf in
    File.aff_file f;
    print_string "\n";
    File.print_consts f;
    (try Printf.printf "Affects are correct : %b\n" (File.check_affect f)
    with File.Affect_Not_Correct(str_list,i_list) ->
        Printf.printf "Affects are not correct :( \nForbiden list : %s"  
            (List.fold_right (fun s acc -> (s^";"^acc)) str_list "");
        Printf.printf "Instruction List : " ;
        File.aff_instr_list [] i_list
    )
                
