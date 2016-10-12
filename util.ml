let sprint_pos start_pos end_pos =
    let start_lnum = start_pos.Lexing.pos_lnum in
    let end_lnum = end_pos.Lexing.pos_lnum in
    if start_lnum == end_lnum then
        let start_cnum = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
        let end_cnum = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
        Printf.sprintf "Line %d, characters %d-%d:\n" start_lnum start_cnum end_cnum
    else
        Printf.sprintf "Line %d-%d:\n" start_lnum end_lnum

