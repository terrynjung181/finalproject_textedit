open OUnit2
open Function
open Command
open State
exception UnknownWord

(* Testing plan: We are using glass box to develop test cases. Search, Command, 
replacing after search, deleting text, cutting line and inserting text are 
automatically tested by OUnit. Search, replace, delete text, and insert test 
cut line are from function.ml and Command is from command.ml. The cursor 
movement from state.ml, saving a file, deleting a file, renaming a file, 
creating a new file and duplicating a file from function.ml are all manully 
tested in our eidtor after the user goes in. This way of testing demonstrates 
the correctness of the system in the best way because the parts that are 
tested manually are easist to test when a user is interacting with our 
system as an actual editor so they can see the real changes made to the file
 in their local disk.*)

let search_key 
    (name : string)
    (input1 : string list) 
    (input2: string) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      match (search input1 input2) with
      |(x,y) -> assert_equal expected_output x ~printer:string_of_int)

let search_value
    (name : string)
    (input1 : string list) 
    (input2: string) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      match (search input1 input2) with
      |(x,y) -> assert_equal expected_output y ~printer:string_of_int)

let search_exception
    (name : string)
    (input1 : string list)
    (input2 : string) : test =
  name >:: (fun _ ->
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_raises (Function.UnknownWord) (fun () -> search input1 input2)
    )

let parse_command
    (name : string)
    (input : string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse input))

let delete_text_test
(name: string)
(input1: string list)
(input2: State.cursor)
(expected_output:string list) : test =
name >:: (fun _ -> 
(* the [printer] tells OUnit how to convert the output to a string *)
assert (expected_output = (delete_text input1 input2)))

let insert_text_test
(name: string)
(input1: string list)
(input2: string)
(input3: State.cursor)
(expected_output:string list) : test =
name >:: (fun _ -> 
(* the [printer] tells OUnit how to convert the output to a string *)
assert_equal expected_output (insert_text input1 input2 input3))

let replace_text_test
(name: string)
(input1: string list)
(input2: string)
(input3: string)
(expected_output:string list) : test =
name >:: (fun _ -> 
(* the [printer] tells OUnit how to convert the output to a string *)
assert_equal expected_output (replace input1 input2 input3))

let cut_line_test
(name: string)
(input1: string list)
(input2: State.cursor)
(expected_output:string list) : test =
name >:: (fun _ -> 
(* the [printer] tells OUnit how to convert the output to a string *)
assert_equal expected_output (cut_line input1 input2))

let search_key_tests = 
  [
    search_key "test_key_a" ["I am Yaoyao.";"Who are you?"] "yao" 0;
    search_key "test_key_b" ["I am Yaoyao.";"Who are you?"] "Ya" 0;
    search_key "test_key_c" ["I am Yaoyao.";"Who are you?"] "I" 0;
    search_key "test_key_d" ["I am Yaoyao.";"Who are you?"] "m y" 0;
    search_key "test_key_e" ["I am Yaoyao.";"Who are you?"] "oya" 0;
    search_key "test_key_f" ["I am Yaoyao.";"Who are you?"] "o." 0;
    search_key "test_key_g" ["I am Yaoyao.";"Who are you?"] "am ya" 0;
    search_key "test_key_h" ["I am Yaoyao.";"Who are you?"] "yaoyao" 0;
    search_key "test_key_i" ["I am Yaoyao.";"Who are you?"] "i a" 0;
    search_key "test_key_j" ["I am Yaoyao.";"Who are you?"] "am Y" 0;
    search_key "test_key_k" ["I am Yaoyao.";"Who are you?"] "i am" 0;
    search_key "test_key_l" ["I am Yaoyao.";"Who are you?"] "aoy" 0;
    search_key "test_key_m" ["I am Yaoyao.";"Who are you?"] "I AM" 0;
    search_key "test_key_n" ["I am Sandy!";"How are you?"] "sandy" 0;
    search_key "test_key_o" ["I am Sandy!";"How are you?"] "how" 1;
    search_key "test_key_p" ["I am Sandy!";"How are you?"] "ArE" 1;
    search_key "test_key_q" ["I am Sandy!";"How are you?"] "i am san" 0;
    search_key "test_key_r" ["I am Sandy!";"How are you?"] "?" 1;
    search_key "test_key_s" ["I am Sandy!";"How are you?"] "!" 0;
    search_key "test_key_t" ["I am Sandy!";"How are you?"] "A" 0;
    search_key "test_key_u" ["I am Sandy!";"How are you?"] "dy!" 0;
    search_key "test_key_v" ["I am Sandy!";"How are you?"] "aRE Y" 1;
    search_key "test_key_w" ["I am Sandy!";"How are you?"] "i" 0;
    search_key "test_key_x" ["I am Sandy!";"How are you?"] "how are " 1;
    search_key "test_key_y" ["I am Sandy!";"How are you?"] "I am" 0;
    search_key "test_key_z" ["I am Sandy!";"How are you?"] "d" 0;

  ]

let search_value_tests = 
  [
    search_value "test_value_a" ["I am Yaoyao.";"Who are you?"] "am" 2;
    search_value "test_value_b" ["I am Yaoyao.";"Who are you?"] "i" 0;
    search_value "test_value_c" ["I am Yaoyao.";"Who are you?"] "YAO" 5;
    search_value "test_value_d" ["I am Yaoyao.";"Who are you?"] "M" 3;
    search_value "test_value_e" ["I am Yaoyao.";"Who are you?"] "o" 7;
    search_value "test_value_f" ["I am Yaoyao.";"Who are you?"] "ao" 6;
    search_value "test_value_g" ["I am Yaoyao.";"Who are you?"] "who" 0;
    search_value "test_value_h" ["I am Yaoyao.";"Who are you?"] "are" 4;
    search_value "test_value_i" ["I am Yaoyao.";"Who are you?"] "?" 11;
    search_value "test_value_j" ["I am Yaoyao.";"Who are you?"] " " 1;
    search_value "test_value_k" ["I am Yaoyao.";"Who are you?"] "YOU?" 8;
    search_value "test_value_l" ["I am Sandy!";"How are you?"] "S" 5;
    search_value "test_value_m" ["I am Sandy!";"How are you?"] "am" 2;
    search_value "test_value_n" ["I am Sandy!";"How are you?"] "SANDY" 5;
    search_value "test_value_o" ["I am Sandy!";"How are you?"] "!" 10;
    search_value "test_value_p" ["I am Sandy!";"How are you?"] " " 1;
    search_value "test_value_q" ["I am Sandy!";"How are you?"] "you?" 8;
    search_value "test_value_r" ["I am Sandy!";"How are you?"] "are" 4;
    search_value "test_value_s" ["I am Sandy!";"How are you?"] "A" 2;
    search_value "test_value_t" ["I am Sandy!";"How are you?"] "AN" 6;
    search_value "test_value_u" ["I am Sandy!";"How are you?"] "u?" 10;
    search_value "test_value_v" ["I am Sandy!";"How are you?"] "m s" 3;
    search_value "test_value_w" ["I am Sandy!";"How are you?"] "are" 4;
    search_value "test_value_x" ["I am Sandy!";"How are you?"] "I a" 0;
    search_value "test_value_y" ["I am Sandy!";"How are you?"] "how are" 0;
    search_value "test_value_z" ["I am Sandy!";"How are you?"] "i am " 0;
  ]

let search_exc_tests = 
  [
    search_exception "test_exc_a" ["I am Yaoyao.";"Who are you?"] 
    "dsafdfadsfrf22f222f2f2ff2f2f2f22f2fd";
    search_exception "test_exc_b" ["I am Yaoyao.";"Who are you?"] 
    "ekrnfadf23fegdbfdgfdlfgfdgfdgfg3l";
    search_exception "test_exc_c" ["I am Yaoyao.";"Who are you?"] 
    "1212dfadfewf22f2f2f2f2f2ff2f2e2fe212";
    search_exception "test_exc_d" ["I am Yaoyao.";"Who are you?"] 
    "r2sdf23ffefgawergagrwgagagaf22";
    search_exception "test_exc_e" ["I am Yaoyao.";"Who are you?"] 
    "dfgdsfasfdfadsfadfasdffafsdfagag";
    search_exception "test_exc_f" ["I am Yaoyao.";"Who are you?"] 
    "e3fadfdafdfadsfdsfasfadsfadsfe2rfd";
    search_exception "test_exc_g" ["I am Yaoyao.";"Who are you?"] 
    "fdjf3g3g3grerhrtegsafdgagaggfaagadfj";
    search_exception "test_exc_h" ["I am Yaoyao.";"Who are you?"] 
    "ffdfdfltm43kgmelrwkmglerwmglwegewf2";
    search_exception "test_exc_i" ["I am Yaoyao.";"Who are you?"]
    "dfdam,fg;dkp32roewijgoigrogjwewlgjfl";
    search_exception "test_exc_j" ["I am Yaoyao.";"Who are you?"] 
    "2er2dfasggerqgregeqrgqgwrgqwrlm";
    search_exception "test_exc_k" ["I am Yaoyao.";"Who are you?"] 
    "ef2eggqkermbf;m;bfbmfd;bmsdf;bmffe";
    search_exception "test_exc_l" ["I am Yaoyao.";"Who are you?"] 
    "2fdaffadfgdafdfdaffasfasf3r l2n";
    search_exception "test_exc_m" ["I am Yaoyao.";"Who are you?"] 
    "qwerdafdafddafadfasfffdaflqkef2";
    search_exception "test_exc_n" ["I am Yaoyao.";"Who are you?"] 
    "eqqfadsfadsfdafdfasdfadsfadfeww";
    search_exception "test_exc_o" ["I am Yaoyao.";"Who are you?"] 
    "ffdfasfdafaergerlkgkneqwrglejgflwq2f";
    search_exception "test_exc_p" ["I am Yaoyao.";"Who are you?"] 
    "fqeqefasdfdfadfdffaffffaffewfv";
    search_exception "test_exc_q" ["I am Yaoyao.";"Who are you?"] 
    "32frqafadfadfr2fghtbfmkmhltrhhghfgjh23f";
    search_exception "test_exc_r" ["I am Yaoyao.";"Who are you?"] 
    "fqwdsafasdfdagqrggi43j2gi3epgefqew";
    search_exception "test_exc_s" ["I am Yaoyao.";"Who are you?"] 
    "dfadfdsfdao4tog4goerobgjgoaafa";
    search_exception "test_exc_t" ["I am Yaoyao.";"Who are you?"] 
    "f3f;dgrewlgjr3qojhposJjgagq2;4f";
    search_exception "test_exc_u" ["I am Yaoyao.";"Who are you?"] 
    "fafl;ddsfadsflanfsdaf;asfgafd;s";
    search_exception "test_exc_v" ["I am Yaoyao.";"Who are you?"] 
    "23rfsafdslfjflasdjfjoq3gjorjoj2322";
    search_exception "test_exc_w" ["I am Yaoyao.";"Who are you?"] 
    "fafafddajklfnafndafdsafkkfkafkasdfsa";
    search_exception "test_exc_x" ["I am Yaoyao.";"Who are you?"] 
    "dafafdffadfgghrt  n nddjfdfffa";
    search_exception "test_exc_y" ["I am Yaoyao.";"Who are you?"] 
    "dfafioqjwgiojjgijsggjajgasdsff";
    search_exception "test_exc_z" ["I am Yaoyao.";"Who are you?"] 
    "fafdag'dsgadgadgafgagjkah3hgohhawrf2";
  ]

let parse_tests =
  [
    parse_command "parse_c" "   quit    " (Quit);
    parse_command "parse_d" "    save " (Save); 
    parse_command "parse_e" "   help    " (Help);
    parse_command "parse_f" "duplicate    " (Duplicate);
  ]

let cursor_a =
  update_cursor (0) (2) 
  
let cursor_b =
  update_cursor (2) (1) 
  
let cursor_c =
  update_cursor (1) (2)
  
let cursor_d =
  update_cursor (3) (2) 

let cursor_e =
  update_cursor (0) (4) 
  
let cursor_f =
  update_cursor (2) (7) 
  
let cursor_g =
  update_cursor (1) (2)
  
let cursor_h =
  update_cursor (3) (6) 

let cursor_i =
  update_cursor (0) (9) 
  
let cursor_j =
  update_cursor (2) (1) 
  
let cursor_k =
  update_cursor (1) (4)
  
let cursor_l =
  update_cursor (3) (5) 

let cursor_m =
  update_cursor (0) (8) 
  
let cursor_n =
  update_cursor (2) (8) 
  
let cursor_o =
  update_cursor (1) (6)
  
let cursor_p =
  update_cursor (3) (3) 

let cursor_q =
  update_cursor (0) (1) 
  
let cursor_r =
  update_cursor (2) (1) 
  
let cursor_s =
  update_cursor (1) (1)
  
let cursor_t =
  update_cursor (3) (2) 

let delete_tests =
  [
    delete_text_test "test_delete_a" ["I am Yaoyao.";"Who are you?"] cursor_a
    ["Iam Yaoyao.";"Who are you?"];
    delete_text_test "test_delete_b" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_b
    ["How are you doing?";"We love to eat!";"ahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_c" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_c
    ["How are you doing?";"W love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_d" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_d
    ["How are you doing?";"We love to eat!";"Hahahahah!";"Tis is 3110!"] ;
    delete_text_test "test_delete_e" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_e
    ["Howare you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_f" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_f
    ["How are you doing?";"We love to eat!";"Hahahaah!";"This is 3110!"] ;
    delete_text_test "test_delete_g" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_g
    ["How are you doing?";"W love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_h" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_h
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This s 3110!"] ;
    delete_text_test "test_delete_i" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_i
    ["How are ou doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_j" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_j
    ["How are you doing?";"We love to eat!";"ahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_k" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_k
    ["How are you doing?";"We ove to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_l" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_l
    ["How are you doing?";"We love to eat!";"Hahahahah!";"Thisis 3110!"] ;
    delete_text_test "test_delete_m" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_m
    ["How areyou doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_n" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_n
    ["How are you doing?";"We love to eat!";"Hahahahh!";"This is 3110!"] ;
    delete_text_test "test_delete_o" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_o
    ["How are you doing?";"We loe to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_p" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_p
    ["How are you doing?";"We love to eat!";"Hahahahah!";"Ths is 3110!"] ;
    delete_text_test "test_delete_q" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"]  
    cursor_q
    ["ow are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_r" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_r
    ["How are you doing?";"We love to eat!";"ahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_s" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_s
    ["How are you doing?";"e love to eat!";"Hahahahah!";"This is 3110!"] ;
    delete_text_test "test_delete_t" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_t
    ["How are you doing?";"We love to eat!";"Hahahahah!";"Tis is 3110!"] ;

  ]

let insert_tests =
  [
    insert_text_test "test_insert_a" ["I am Yaoyao.";"Who are you?"] 
    "hihdidi" cursor_a ["I hihdidiam Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_b" ["I am Yaoyao.";"Who are you?"] 
    "not" cursor_a ["I notam Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_c" ["I am Yaoyao.";"Who are you?"] 
    "dd" cursor_a ["I ddam Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_d" ["I am Yaoyao.";"Who are you?"] 
    "345" cursor_a ["I 345am Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_e" ["I am Yaoyao.";"Who are you?"] 
    "ddddd" cursor_a ["I dddddam Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_f" ["I am Yaoyao.";"Who are you?"] 
    "  " cursor_a ["I   am Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_g" ["I am Yaoyao.";"Who are you?"] 
    "" cursor_a ["I am Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_h" ["I am Yaoyao.";"Who are you?"] 
    "!" cursor_a ["I !am Yaoyao.";"Who are you?"];
    insert_text_test "test_insert_i" ["I am Yaoyao.";"Who are you?";"aa"] 
    "not" cursor_b ["I am Yaoyao.";"Who are you?";"anota"];
    insert_text_test "test_insert_j" ["I am Yaoyao.";"Who are you?";"aa"] 
    "123" cursor_b ["I am Yaoyao.";"Who are you?";"a123a"];
    insert_text_test "test_insert_k" ["I am Yaoyao.";"Who are you?";"aa"] 
    "  " cursor_b ["I am Yaoyao.";"Who are you?";"a  a"];
    insert_text_test "test_insert_l" ["I am Yaoyao.";"Who are you?";"aa"]
    "" cursor_b ["I am Yaoyao.";"Who are you?";"aa"];
    insert_text_test "test_insert_m" ["I am Yaoyao.";"Who are you?";"aa"] 
    "d " cursor_b ["I am Yaoyao.";"Who are you?";"ad a"];
    insert_text_test "test_insert_n" ["I am Yaoyao.";"Who are you?";"aa"] 
    "     " cursor_b ["I am Yaoyao.";"Who are you?";"a     a"];
    insert_text_test "test_insert_o" ["I am Yaoyao.";"Who are you?";"aa"] 
    "??" cursor_b ["I am Yaoyao.";"Who are you?";"a??a"];
    insert_text_test "test_insert_p" ["I am Yaoyao.";"Who are you?";"aa"] 
    "@" cursor_b ["I am Yaoyao.";"Who are you?";"a@a"];
    insert_text_test "test_insert_q" ["I am Yaoyao.";"Who are you?";"aa"] 
    "vvv b  " cursor_b ["I am Yaoyao.";"Who are you?"; "avvv b  a"];
    insert_text_test "test_insert_r" ["I am Yaoyao.";"Who are you?";"aa"] 
    "nope" cursor_b ["I am Yaoyao.";"Who are you?";"anopea"];
  ]

let replace_tests = 
  [
    replace_text_test "test_replace_a" ["I am Yaoyao.";"Who are you?"] "yao" 
    "ha" ["I am hayao.";"Who are you?"];
    replace_text_test "test_replace_b" ["I am Yaoyao.";"Who are you?"] "are" 
    "123" ["I am Yaoyao.";"Who 123 you?"];
    replace_text_test "test_replace_c" ["I am Yaoyao.";"Who are you?"] "." 
    "xixi" ["I am Yaoyaoxixi";"Who are you?"];
    replace_text_test "test_replace_d" ["I am Yaoyao.";"Who are you?"] "?" 
    "??" ["I am Yaoyao.";"Who are you??"];
    replace_text_test "test_replace_e" ["I am Yaoyao.";"Who are you?"] "o" 
    "aa" ["I am Yaaayao.";"Who are you?"];
    replace_text_test "test_replace_f" ["I am Yaoyao.";"Who are you?"] "yaoyao" 
    "sssbb" ["I am sssbb.";"Who are you?"];
    replace_text_test "test_replace_g" ["I am Yaoyao.";"Who are you?"] "who" 
    "laughing" ["I am Yaoyao.";"laughing are you?"];
    replace_text_test "test_replace_h" ["I am Yaoyao.";"Who are you?"] "o." 
    "" ["I am Yaoya";"Who are you?"];
    replace_text_test "test_replace_i" ["I am Yaoyao.";"Who are you?"] "o a" 
    "oa" ["I am Yaoyao.";"Whoare you?"];
    replace_text_test "test_replace_k" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "san" "An" ["They are Andy and Terryn!";"How are you?"];
    replace_text_test "test_replace_l" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "terr" "ha" ["They are Sandy and hayn!";"How are you?"];
    replace_text_test "test_replace_n" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "how" "heloow" ["They are Sandy and Terryn!";"heloow are you?"];
    replace_text_test "test_replace_o" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "!" "!!!" ["They are Sandy and Terryn!!!";"How are you?"];
    replace_text_test "test_replace_p" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "y" "yyy" ["Theyyy are Sandy and Terryn!";"How are you?"];
    replace_text_test "test_replace_q" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "are" "am" ["They am Sandy and Terryn!";"How are you?"];
    replace_text_test "test_replace_r" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "are you" "is he" ["They are Sandy and Terryn!";"How is he?"];
    replace_text_test "test_replace_s" 
    ["They are Sandy and Terryn!";"How are you?"] 
    "they" "Them" ["Them are Sandy and Terryn!";"How are you?"];
  ]
  
let cut_tests =
  [
    cut_line_test "test_cut_a" 
    ["I am Yaoyao.";"Who are you?";"";"She is Sandy?"] cursor_a
    ["Who are you?";"";"She is Sandy?"];
    cut_line_test "test_cut_b" 
    ["I am Yaoyao.";"Who are you?";"";"She is Sandy?"] cursor_b
    ["I am Yaoyao.";"Who are you?";"She is Sandy?"];
    cut_line_test "test_cut_c" 
    ["I am Yaoyao.";"Who are you?";"";"She is Sandy?"] cursor_c
    ["I am Yaoyao.";"";"She is Sandy?"];
    cut_line_test "test_cut_d" 
    ["I am Yaoyao.";"Who are you?";"";"She is Sandy?"] cursor_d
    ["I am Yaoyao.";"Who are you?";""];
    cut_line_test "test_cut_e" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_a ["We love to eat!";"Hahahahah!";"This is 3110!"];
    cut_line_test "test_cut_f" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_b ["How are you doing?";"We love to eat!";"This is 3110!"];
    cut_line_test "test_cut_g" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_c ["How are you doing?";"Hahahahah!";"This is 3110!"];
    cut_line_test "test_cut_h" 
    ["How are you doing?";"We love to eat!";"Hahahahah!";"This is 3110!"] 
    cursor_d ["How are you doing?";"We love to eat!";"Hahahahah!"];
    cut_line_test "test_cut_i" 
    ["When I say you’re my life,"; "I hope you know that it’s true,";
    "When I tell you you’re my world,"; "know that I only have eyes for you."] 
    cursor_a
    ["I hope you know that it’s true,";"When I tell you you’re my world,"; 
    "know that I only have eyes for you."];
    cut_line_test "test_cut_j"     
    ["When I say you’re my life,"; "I hope you know that it’s true,";
    "When I tell you you’re my world,"; "know that I only have eyes for you."]  
    cursor_b
    ["When I say you’re my life,"; "I hope you know that it’s true,";
    "know that I only have eyes for you."] 
  ]

let suite =
  "test suite for project"  >::: List.flatten [
    search_value_tests;
    search_key_tests;
    search_exc_tests;
    parse_tests;
    delete_tests;
    insert_tests;
    replace_tests;
    cut_tests;
    ]
  
  let _ = run_test_tt_main suite