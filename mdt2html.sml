fun int_to_char(a : int ) = 
  if a=0 then #"0" 
  else if a=1 then #"1"
  else if a=2 then #"2"
  else if a=3 then #"3"
  else if a=4 then #"4"
  else if a=5 then #"5"
  else if a=6 then #"6"
  else if a=7 then #"7"
  else if a=8 then #"8"
  else #"9"

fun char_to_int(a : char) = 
  if a= #"0" then 0 
  else if a= #"1" then 1 
  else if a= #"2" then 2 
  else if a= #"3" then 3 
  else if a= #"4" then 4 
  else if a= #"5" then 5 
  else if a= #"6" then 6 
  else if a= #"7" then 7 
  else if a= #"8" then 8 
  else 9

fun remove_bs_line(l,ans)=
  if null(l) then ans 
  else if ord(hd(l)) = 92 andalso not(null(tl(l))) then remove_bs_line(tl(tl(l)), hd(tl(l)) :: ans)
  else remove_bs_line(tl(l), hd((l)) :: ans)

fun remove_bs_string(s) = implode(rev(remove_bs_line(explode(s),[])))
fun remove_bs_file(L,ans)=
  if null(L) then ans 
  else remove_bs_file(tl(L),remove_bs_string(hd(L)) :: ans ) 

fun remove_bs_finally(L)= rev(remove_bs_file(L,[]))
fun change_tab_to_line(l,ans)=
  if null(l) then ans 
  else if hd(l) = #"\t" then change_tab_to_line(tl(l), #" " :: #" " :: #" " :: #" " :: ans) 
  else change_tab_to_line(tl(l), hd(l) :: ans) 

fun change_string_tab(s) = implode(rev(change_tab_to_line(explode(s),[])))
fun change_tab_to_spaces(L,ans)=
  if null(L) then ans 
  else change_tab_to_spaces(tl(L),change_string_tab(hd(L)) :: ans)

fun file_tab_to_spaces(L) = rev(change_tab_to_spaces(L,[]))
fun is_digit(x)=
  if (ord(x)>= 48 andalso ord(x)<=57) then true 
  else false 

fun first_non_empty_word(l,ans,boolu,bool_start)=
  if null(l) then ans 
  else if not boolu then ans 
  else if bool_start andalso hd(l)<> #" " then first_non_empty_word(tl(l),hd(l)::ans,boolu,not bool_start)
  else if bool_start andalso hd(l)= #" " then first_non_empty_word(tl(l),ans,boolu, bool_start)
  else if hd(l)<> #" " then first_non_empty_word(tl(l),hd(l)::ans,boolu,bool_start)
  else first_non_empty_word(tl(l),ans,not boolu,bool_start)

fun first_non_empty_word_s(s) = implode(rev(first_non_empty_word(explode(s),[],true,true)))
fun is_code_block(l)= 
if (not(null(tl(l)))) andalso (not(null(tl(tl(l))))) andalso (not(null(tl(tl(tl(l)))))) andalso (not(null(tl(tl(tl(tl(l))))))) andalso (not(null(tl(tl(tl(tl(tl(l)))))))) andalso (not(null(tl(tl(tl(tl(tl(tl(l))))))))) andalso (hd(l) = #" ") andalso (hd(tl(l)) = #" ") andalso (hd(tl(tl(l))) = #" ") andalso (hd(tl(tl(tl(l)))) = #" ") andalso (hd(tl(tl(tl(tl(l))))) = #" ") andalso (hd(tl(tl(tl(tl(tl(l)))))) = #" ") andalso (hd(tl(tl(tl(tl(tl(tl(l))))))) = #" ") then true 
else false 

fun remove_spaces(l,ans)=
if null(l) then ans 
else if hd(l) = #" " then remove_spaces(tl(l),ans)
else remove_spaces(tl(l),hd(l)::ans)

fun remove_spaces_string(s)=
implode(rev(remove_spaces(explode(s),[])))

fun is_code_block_string(s) = 
let val l = explode(s)
in 
is_code_block(l) 
end 

fun code_block_change(l,ans)=
if null(l) then ans
else if hd(l)= #">" then code_block_change(tl(l),#";" :: #"t" :: #"g" :: #"&" :: ans)
else if hd(l)= #"<" then code_block_change(tl(l),#";" :: #"t" :: #"l" :: #"&" :: ans)
else code_block_change(tl(l),hd(l) :: ans) 

fun code_block_change_string(s)=
"<pre><code>"^implode(rev(code_block_change(explode(s),[])))^"</code></pre>"


fun try1(inputfile) = 
  let val check=TextIO.endOfStream(inputfile)
      val line1=TextIO.inputLine(inputfile)
      val line=if check then "" else valOf(line1)
  in 
      if check then [] 
      else line::try1(inputfile)
  end

fun try_heading(l1,i,ans)=
if hd(l1) = #"#" then try_heading(tl(l1),i+1,ans)
else if i=0 then l1 
else 
let 
val temp1 = (#"<" :: #"h" :: int_to_char(i) :: #">" :: (l1))
val temp2 = rev temp1 
val temp3  = (#">" :: int_to_char(i):: #"h" :: #"/" :: #"<" :: (temp2))
in 
rev temp3
end 

fun result_after_header(L)=
if null(L) then []
else if is_code_block_string(hd(L)) then code_block_change_string(hd(L))::result_after_header(tl(L)) 
else implode(try_heading(explode(hd(L)),0,[]))::result_after_header(tl(L)) 



fun try_paragraph(L) =
if null(L) then [] 
else if hd(L)="\n" then "</p><p>\n"::try_paragraph(tl(L)) 
else hd(L)::try_paragraph(tl(L))

fun result_after_para(L)=
let val temp1 = "<p>\n"::try_paragraph(L) 
in 
temp1@["</p>\n"]
end 

fun countt(L : char list, x : char,i)=
if null(L) then i 
else if not(hd(L)=x) then i 
else countt(tl(L),x,i+1)

fun insert_block_quote_at_front(k,L)=
if k=0 then L 
else insert_block_quote_at_front(k-1, #"<" :: #"b" :: #"l" :: #"o" :: #"c" :: #"k" :: #"q" :: #"u" :: #"o" :: #"t" :: #"e" :: #">" :: L)

fun insert_block_quote_at_last(k,L)=
if k=0 then rev L 
else insert_block_quote_at_last(k-1, #"\n" :: #">" :: #"e" :: #"t" :: #"o" :: #"u" :: #"q" :: #"k" :: #"c" :: #"o" :: #"l" :: #"b" :: #"/" :: #"<" ::L)

fun x_tail(x, L)=
if x=0 then L 
else x_tail(x-1,tl(L))


fun replace_by_tdtr(L,ans)=
if null(L) then ans 
else if ord(hd(L)) = 92 andalso not(null(tl(L))) then replace_by_tdtr(tl(tl(L)),hd(tl(L))::hd(L)::ans)
else if hd(L) = #"|" then replace_by_tdtr(tl(L), #">" :: #"D" :: #"T" :: #"<" :: #">" :: #"D" :: #"T" :: #"/" :: #"<" :: ans)
else replace_by_tdtr(tl(L),hd(L)::ans)

fun replace_in_table(s) = 
let val l = explode(s) 
in 
implode(rev (replace_by_tdtr(l,[])))
end 

fun try_table(L,ans,bool_table) = 
if null(L) then ans
else 
if (not bool_table) andalso (hd(L) = "<<\n" orelse hd(L) = "<< \n") then try_table(tl(L),"<CENTER><TABLE border = \"1\" >" :: ans,not bool_table)
else if (bool_table) andalso (hd(L) = ">>\n" orelse hd(L) = ">> \n") then try_table(tl(L), "</TABLE></CENTER>" :: ans , not bool_table)
else if (bool_table) then 
let val x = replace_in_table(hd(L))
val y = "<TR><TD>" ^ x ^ "</TD></TR>"
in 
try_table(tl(L),y::ans , bool_table)
end 
else 
try_table(tl(L),hd(L) :: ans,bool_table)


fun result_after_table(L) = 
rev (try_table(L,[],false))

fun try_blockquote(L,ans) = 
if null(L) then ans 
else 
let val temp1 = hd(L)
    val temp2 = explode(temp1)
    val tt1 =  countt(temp2, #">",0)
    in 
    if (tt1 = 0) then try_blockquote(tl(L),hd(L)::ans)
    else 
    let 
        val rrr = x_tail(tt1,temp2)
        val rr1 = insert_block_quote_at_front(tt1,rrr) 
        val rr2 = insert_block_quote_at_last(tt1,rev rr1)
        val rr3= implode(rr2) 
        in
        try_blockquote(tl(L),rr3::ans)
        end 
    end 

fun result_after_blockquote(L)=
rev (try_blockquote(L,[]))

fun simple_link_line_aux(l,ans : char list, temp_list, bool_l)=
if null(l) then ans 
else if bool_l then 
if hd(l) = #">" then 
simple_link_line_aux(tl(l),rev(explode("</a>"))@(temp_list) @(hd(l):: #"\"" ::ans), [],not bool_l) 
else simple_link_line_aux(tl(l),hd(l)::ans, hd(l)::temp_list,bool_l)
else if (not(null(tl(l)))) andalso (not(null(tl(tl(l))))) andalso (not(null(tl(tl(tl(l)))))) andalso (not(null(tl(tl(tl(tl(l))))))) andalso (hd(l) = #"<") andalso (hd(tl(l)) = #"h") andalso (hd(tl(tl(l))) = #"t") andalso (hd(tl(tl(tl(l)))) = #"t")  andalso (hd(tl(tl(tl(tl(l))))) = #"p") then 
simple_link_line_aux(tl(l),rev(explode("<a href = \"")) @ans ,temp_list, not bool_l)
else simple_link_line_aux(tl(l),(hd(l) :: ans), temp_list, bool_l)

fun after_link(s)= 
let val l = explode(s)
in 
implode(rev (simple_link_line_aux(l, [] ,[],false)))
end  


fun simple_link(L,ans)=
if null(L) then ans 
else if is_code_block_string(hd(L)) then simple_link(tl(L),(hd(L))::ans)
else simple_link(tl(L),after_link(hd(L))::ans)

fun result_after_simple_link(L)=
rev (simple_link(L,[]))


fun horizontal_ruling(l,ans)=
if null(l) then ans 
else if ord(hd(l)) = 92 andalso not(null(l)) then horizontal_ruling(tl(tl(l)),hd(tl(l)) :: hd(l) :: ans)
else if (not(null(tl(l)))) andalso (not(null(tl(tl(l))))) andalso hd(l) = #"-" andalso hd(tl(l)) = #"-" andalso hd(tl(tl(l))) = #"-" then 
horizontal_ruling(tl(tl(tl(l))),#">" :: #"r" :: #"h" :: #"<" :: ans)
else horizontal_ruling(tl(l),hd(l)::ans)


fun horizontal_ruling_for_line(s)=
let val l=explode(s) in 
implode(rev(horizontal_ruling(l,[])))
end 

fun try_horizontal_ruling(L,ans)=
if null(L) then ans 
else if is_code_block_string(hd(L)) then try_horizontal_ruling(tl(L),(hd(L))::ans)
else try_horizontal_ruling(tl(L),horizontal_ruling_for_line(hd(L))::ans)

fun result_after_hr(L)=
rev(try_horizontal_ruling(L,[]))

fun add_li(l,ans,bool2)=
if null(l) then ans 
else 
if not bool2 then
if hd(l)= #" " then 
add_li(tl(l),rev(explode("<li>\n"))@ans,not bool2)
else 
add_li(tl(l),ans, bool2)
else 
add_li(tl(l),hd(l)::ans, bool2)

fun add_li_to_s(s)=
let val l = explode(s)
in implode(rev(add_li(l,[],false)))
end

fun is_first_word_nice(l,b,is_space)=
if null(l) then b
else 
if is_space then b
else if is_digit(hd(l)) then  is_first_word_nice(tl(l),b,is_space)
else if b andalso not (null(tl(l))) andalso hd(l)= #"." andalso hd(tl(l)) = #" " then true 
else false 

fun nice_for_string(s)=
is_first_word_nice(explode(s),true,false)

fun ordered_list(L,ans,booli)=
if null(L) then ans 
else if booli andalso not (null(tl(L))) andalso ((remove_spaces_string(hd(L)) = "</p><p>\n")) andalso ((remove_spaces_string(hd(tl(L))) = "</p><p>\n")) then ordered_list(tl(L),"</ol>\n" :: ans,not booli)
else if booli andalso nice_for_string(hd(L)) then ordered_list(tl(L),add_li_to_s(hd(L))::ans,booli)
else if booli then ordered_list(tl(L),hd(L)::ans,booli)
else if nice_for_string(hd(L)) then ordered_list((L),"<ol>\n":: ans,not booli)
else ordered_list(tl(L),hd(L)::ans,booli)

fun result_after_ol(L)=
rev(ordered_list(L,[],false))

fun is_good(s)=
let val L = explode(s) 
in
if not (null(L)) andalso not (null(tl(L))) andalso hd(L)= #"-" andalso hd(tl(L)) = #" " then true 
else false 
end 


fun add_li_in_ul(s)= 
implode(explode("<li>\n") @tl(tl(explode(s))))

fun unordered_list(L,ans,booli)=
if null(L) then ans 
else if booli andalso is_good(hd(L)) then unordered_list(tl(L),add_li_in_ul(hd(L)) :: ans ,booli)
else if booli then unordered_list(tl(L),hd(L)::ans,booli)
else if booli andalso not (null(tl(L))) andalso ((remove_spaces_string(hd(L)) = "</p><p>\n")) andalso ((remove_spaces_string(hd(tl(L))) = "</p><p>\n")) then unordered_list(tl(L),"</ul>\n"::ans,booli)
else if is_good(hd(L)) then unordered_list((L),"<ul>\n":: ans,not booli)
else unordered_list(tl(L),hd(L)::ans,booli)

fun result_after_ul(L)=
rev(unordered_list(L,[],false))


fun bi_line(l,ans,boolb,booli)=
if null(l) then (ans, boolb,booli)
else if ord(hd(l)) = 92 andalso not(null(tl(l))) then bi_line(tl(tl(l)), hd(tl(l)) :: hd(l) :: ans,boolb,booli)
else if not (null(l)) andalso hd(l)= #"*" andalso hd(tl(l)) = #"*" then 
if boolb then bi_line(tl(tl(l)), #">" :: #"b" :: #"/" :: #"<" :: ans,not boolb,booli)
else bi_line(tl(tl(l)),#">" :: #"b" :: #"<" :: ans,not boolb,booli)
else if hd(l) = #"*" then 
if booli then bi_line((tl(l)),#">" :: #"i" :: #"/" :: #"<" :: ans,boolb,not booli)
else 
bi_line((tl(l)),#">" :: #"i" :: #"<" :: ans,boolb,not booli)
else 
bi_line(tl(l),hd(l)::ans,boolb,booli)


fun bi_string(s,boolb,booli)= 
let val v0 = bi_line(explode(s),[],boolb,booli)
val v1 = implode(rev(#1 v0))
val v2= #2 v0 
val v3 = #3 v0
in 
(v1,v2,v3)
end

fun bold_and_italics(L,ans,boolb,booli)=
if null(L) then ans 
else if is_code_block_string(hd(L)) then bold_and_italics(tl(L),hd(L)::ans,boolb,booli)
else 
let val aa = bi_string(hd(L),boolb,booli) 
in 
bold_and_italics(tl(L), ((#1 aa) :: ans) , #2 aa, #3 aa) 
end     

fun result_after_bi(L)= 
rev(bold_and_italics(L,[],false,false))


fun try_under_line(l,ans,boolu, temp)=
if null l then ans 
else if ord(hd(l)) = 92 andalso not(null(l)) then try_under_line(tl(tl(l)),hd(tl(l)) :: hd(l) :: ans, false , [] ) 
else if (hd(l)= #" ") andalso boolu then try_under_line(tl(l),#" " ::(temp@ans), false , [] )
else if (hd(l)= #"\n") andalso boolu then try_under_line(tl(l),#"\n" ::(temp@ans), false , [] )
else if (hd(l)= #" ") andalso not boolu then try_under_line(tl(l), #" " :: ans, false ,[])
else if (hd(l)= #"\n") andalso not boolu then try_under_line(tl(l), #"\n" :: ans, false ,[])
else if hd(l)= #"_" andalso boolu then 
if null(tl(l)) then try_under_line(tl(l), #">" :: #"u" :: #"/" :: #"<" :: (temp@(rev(explode("<u>")))@(ans)), boolu , [])
else try_under_line(tl(l), #">" :: #"u" :: #"/" :: #"<" :: #" " :: (temp@(rev(explode("<u>")))@(ans)), boolu , [])
else if hd(l)= #"_" andalso not boolu then try_under_line(tl(l), (ans), not boolu , temp) 
else if boolu then try_under_line(tl(l), (ans), boolu , hd(l) ::temp) 
else try_under_line(tl(l), hd(l) ::ans, boolu , temp) 

fun try_under_line_string(s) = 
let val l = explode(s) in
implode(rev(try_under_line(l,[],false, [])))
end 

fun result_after_underline_temp(L,ans)=
if null(L) then ans 
else if is_code_block_string(hd(L)) then result_after_underline_temp(tl(L),(hd(L))::ans)
else result_after_underline_temp(tl(L), try_under_line_string(hd(L))::ans)

fun result_after_underline(L)=  
rev(result_after_underline_temp(L,[]))

fun result_after_code_blocks_temp(L,ans)=
if null(L) then ans
else if is_code_block_string(hd(L)) then result_after_code_blocks_temp(tl(L),code_block_change_string(hd(L))::ans)
else result_after_code_blocks_temp(tl(L),(hd(L))::ans)

fun result_after_code_blocks(L)=
rev(result_after_code_blocks_temp(L,[]))

fun square_bracket_link(l,ans,boool_link,bool_text,temp)=
if null(l) then ans 
else if hd(l) = #"[" then square_bracket_link(tl(l),ans,true,bool_text,temp)
else if hd(l) = #"]" then 
if not(null(tl(l))) andalso hd(tl(l))= #"(" then square_bracket_link(tl(tl(l)) ,(#">" :: #"\"" :: (temp @ (#"\"" :: #" " :: #"=" :: #" " :: #"f" :: #"e" :: #"r" :: #"h" :: #" " :: #"a" :: #"<" :: ans))),false,true,[])
else square_bracket_link(tl(l) ,#"]" :: (temp @ (#"[" :: ans)),false,false,[])
else if hd(l) = #")" andalso bool_text then square_bracket_link(tl(l),rev(explode("</a>"))@(temp@ans),false, false, [])
else if boool_link then square_bracket_link(tl(l),ans,boool_link,bool_text,hd(l) :: temp)
else if bool_text then square_bracket_link(tl(l),ans,boool_link,bool_text,hd(l) :: temp)
else square_bracket_link(tl(l),hd(l)::ans,boool_link,bool_text,temp)

fun square_bracket_link_s(s)=
implode(rev(square_bracket_link(explode(s),[],false, false ,[])))

fun square_bracket_file(L,ans)=
if null(L) then ans 
else square_bracket_file(tl(L),square_bracket_link_s(hd(L))::ans)

fun result_after_sq_link(L) =
rev(square_bracket_file(L,[]))

fun write([],output_file) = TextIO.closeOut output_file
  | write(x::xs,output_file) = let val _ = TextIO.output(output_file,x) in write(xs,output_file) end 

fun mdt2html(filename) =
let val input_file = TextIO.openIn(filename^".mdt")
    val L = try1(input_file)
    val input_close = TextIO.closeIn(input_file)
    val mai = result_after_underline(result_after_bi(result_after_ul(result_after_ol(result_after_hr(result_after_sq_link(result_after_simple_link(result_after_header(result_after_para(result_after_blockquote(result_after_table(result_after_code_blocks(file_tab_to_spaces(L)))))))))))))
    val output_file = TextIO.openOut(filename^".html")
in write(mai,output_file)
end
