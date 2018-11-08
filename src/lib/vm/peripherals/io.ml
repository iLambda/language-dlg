open Lwt
open LTerm_event
open CamomileLibrary

(* An input *)
type io_input =
  (* Rush a message / acknowledge *)
  | Enter
  (* Quit VM *)
  | Quit
  (* Directional keys *)
  | Up | Down | Left | Right

(* IO message options *)
type io_msg_option =
  | MsgNoRush
  | MsgNoAcknowledge
type io_msg_options = io_msg_option list

(* A message *)
type io_message = string * io_msg_options

(* A break exception *)
exception Io_break

(* represents an IO channel *)
type io = {
  (* the base speed & speed modifier *)
  basespeed: float;
  mutable speed: float;
  (* a message mailbox *)
  (* message: io_message Lwt_mvar.t; *)
  (* an input key mailbox *)
  input: io_input Lwt_mvar.t;
  (* the terminal and its old mode *)
  terminal: LTerm.t;
}


(* Enable *)
let io_enable io =
  (* character detection *)
  let rec getch () =
    (* clear *)
    let clear () =
      (* Clear mailbox  *)
      if not (Lwt_mvar.is_empty io.input)
      (* empty *)
      then Lwt_mvar.take io.input
           >>= fun _ -> return ()
      (* nah *)
      else return ()
    in
    (* run cpu *)
    LTerm.read_event io.terminal
    (* >>= fun ev -> Lwt_io.printl (LTerm_event.to_string ev) *)
    (* >>= fun ev -> Lwt_io.printl (LTerm_event.to_string ev) *)
    >>= fun ev -> match ev with
                    (* Quit *)
                    | Key { code = Char c; control = true; _ } when c = UChar.of_char 'c' ->
                      (* clear mailbox *)
                      clear ()
                      (* push a break *)
                      >>= fun () -> Lwt_mvar.put io.input Quit
                    (* Enter key *)
                    | Key { code = Enter; _ } ->
                      (* clear mailbox *)
                      clear ()
                      (* push a enter *)
                      >>= fun () -> Lwt_mvar.put io.input Enter
                      (* recur *)
                      >>= fun () -> getch ()
                    (* Up *)
                    | Key { code = Up; _ } ->
                      (* clear mailbox *)
                      clear ()
                      (* push a enter *)
                      >>= fun () -> Lwt_mvar.put io.input Up
                      (* recur *)
                      >>= fun () -> getch ()
                    (* Down *)
                    | Key { code = Down; _ } ->
                      (* clear mailbox *)
                      clear ()
                      (* push a enter *)
                      >>= fun () -> Lwt_mvar.put io.input Down
                      (* recur *)
                      >>= fun () -> getch ()
                    (* nothing *)
                    | _ -> getch ()
  in
  (* Save *)
  LTerm.save_state io.terminal
  (* Set term in raw mode *)
  >>= fun () -> LTerm.enter_raw_mode io.terminal
  (* Show the cursor*)
  >>= fun mode -> LTerm.show_cursor io.terminal
  (* Clear the screen *)
  >>= fun () -> LTerm.clear_screen io.terminal
  (* the char detection *)
  >>= fun () -> ignore (getch ()); return ()
  (* Return the mode *)
  >>= fun () -> return mode

(* Break ? *)
(* let io_break io =
  (* Is there something in the mailbox ? *)
  match Lwt_mvar.take_available io.input with
    (* Nothing. No break *)
    | None -> return false
    (* Something, and it's a quit. *)
    | Some Quit ->
      (* Put back *)
      Lwt_mvar.put io.input Quit
      (* Return true *)
      >>= fun () -> return true
    (* Something, and it's not a quit. *)
    | Some x ->
      (* Put back *)
      Lwt_mvar.put io.input x
      (* Return false *)
      >>= fun () -> return false *)

(* Disable *)
let io_disable io mode =
  (* Load *)
  LTerm.leave_raw_mode io.terminal mode
  (* Restore *)
  >>= fun () -> LTerm.load_state io.terminal

(* create a new io channel *)
let io_make () = {
  (* phyiscal properties *)
  basespeed = 0.05;
  speed = 1.;
  (* Lwt/Lterm data *)
  (* message = Lwt_mvar.create_empty (); *)
  input = Lwt_mvar.create_empty ();
  terminal = Lwt_main.run (Lazy.force LTerm.stdout)
}

(* Do a cleanup *)
let io_cleanup io =
  (* Clear screen *)
  LTerm.clear_screen io.terminal
  (* Flush *)
  >>= fun () -> LTerm.flush io.terminal
  (* Goto 0, 0 *)
  >>= fun () -> LTerm.goto io.terminal { row=0; col=0 }
  (* Clear the mailbox *)
  >>= fun () ->
        (* Clear mailbox  *)
        if not (Lwt_mvar.is_empty io.input)
        (* empty *)
        then Lwt_mvar.take io.input
             >>= fun _ -> return ()
        (* nah *)
        else return ()

(* Displays a message in the VM I/O*)
let io_send_message io opts message =
  (* Write message *)
  let rec write opts s =
    (* Check if there is a break key in the mailbox *)
    match s, (Lwt_mvar.take_available io.input) with
      (* Quit *)
      | _, Some Quit -> raise Io_break
      (* There is no message *)
      | "", _ -> if List.mem MsgNoAcknowledge opts
                 then return ()  (* no acknowledge ; we're done *)
                 else
                  (* ack needed : take from mailbox nd wait *)
                  Lwt_mvar.take io.input
                  (* check return *)
                  >>= (function
                        (* if enter, quit *)
                        | Enter -> return ()
                        (* if quit, break *)
                        | Quit -> raise Io_break
                        (* else, keep watching *)
                        | _ -> write opts "")

      (* There is enter key and there is still a message ; rush to end *)
      | str, Some Enter ->
        (* check if rush is allowed *)
        if List.mem MsgNoRush opts then write opts str
        (* rush IS allowed. *)
        else
          (* print *)
          LTerm.print str
          (* call recursively with empty string, wait for ok *)
          >>= fun () -> write opts ""
      (* Nothing, print a character *)
      | str, _ ->
        (* print the first char *)
        LTerm.print (String.sub str 0 1)
        (* Wait *)
        >>= fun () -> Lwt_unix.sleep (io.basespeed /. io.speed)
        (* Call recursively *)
        >>= fun () -> write opts (String.sub str 1 ((String.length str) - 1))
  in
  (* cleanup *)
  io_cleanup io
  (* write a message *)
  >>= fun () -> write opts message

(* Set the speed *)
let io_set_speed io speed =
  io.speed <- speed

(* A choice *)
let io_ask_choice io choices =
  (* Fold choices *)
  let fold_choices acc choice =
    (* Accumulate *)
    acc ^ "  " ^ choice ^ "\n"
  in
  (* Print choices *)
  let rec write s =
    (* Check if there is a break key in the mailbox *)
    match s, (Lwt_mvar.take_available io.input) with
      (* Quit *)
      | _, Some Quit -> raise Io_break
      (* Choices have all been printed *)
      | "", _ -> return ()
      (* There is enter key and there is still a message ; rush to end *)
      | str, Some Enter ->
          (* print *)
          LTerm.print str
          (* call recursively with empty string, wait for ok *)
          >>= fun () -> write ""
      (* Nothing, print a character *)
      | str, _ ->
        (* print the first char *)
        LTerm.print (String.sub str 0 1)
        (* Wait *)
        >>= fun () -> Lwt_unix.sleep (io.basespeed *. io.speed)
        (* Call recursively *)
        >>= fun () -> write (String.sub str 1 ((String.length str) - 1))
  in
  (* Select a choice *)
  let rec select old current amount =
    (* Remove old *)
    LTerm.goto io.terminal { row=old; col=0 }
    >>= fun () -> LTerm.print " "
    (* Draw new *)
    >>= fun () -> LTerm.goto io.terminal { row=current; col=0 }
    >>= fun () -> LTerm.print ">"
    (* Get a mailbox element *)
    >>= fun () -> Lwt_mvar.take io.input
    (* Check its type *)
    >>= (function
          (* if quit, break *)
          | Quit -> raise Io_break
          (* if enter, quit *)
          | Enter -> return current
          (* if up, increment *)
          | Up -> select current ((current + (amount - 1)) mod amount) amount
          (* if down, increment *)
          | Down -> select current ((current + 1) mod amount) amount
          (* anything else *)
          | _ -> select old current amount)
  in
  (* cleanup *)
  io_cleanup io
  (* Hide cursor *)
  >>= fun () -> LTerm.hide_cursor io.terminal
  (* Print choices *)
  >>= fun () -> write (List.fold_left fold_choices "" choices)
  (* Select a choice *)
  >>= fun () -> select 0 0 (List.length choices)
  (* Show cursor *)
  >>= fun idx -> LTerm.show_cursor io.terminal
  (* Return *)
  >>= fun () -> return idx

  (*List.iteri
    (fun i s -> io_write io ("(" ^ (string_of_int (i+1)) ^  ") " ^ s ^ "\n"))
    choices;
  (* The response *)
  let chosen = (io_get_int 1 ((List.length choices) + 1)) - 1 in
  (* newline *)
  print_newline();
  chosen*)
