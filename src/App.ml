open Graphics;;

type todo = {
  text: string;
  checked: bool;
};;

type state = {
  value: string;
  todo: todo list;
};;

let width = 300;;
let height = 500;;
let item_height = 50;;
let padding = 10;;

open_graph "";;
set_window_title "TodoML";;
resize_window width height;;

let draw_item = fun index item ->
  set_color 0xCCCCCC;
  moveto 0 (height - (index + 1) * item_height);
  lineto width (height - (index + 1) * item_height);
  if item.checked = false then
    set_color 0x333333 else
    set_color 0x00FF00;
  moveto padding (height - (index + 1) * item_height + item_height / 2);
  draw_string item.text;;

let draw_input = fun input ->
  set_color 0x333333;
  moveto padding padding;
  draw_string input;
  (* cursor *)
  draw_rect (padding + (fst (text_size input))) padding 2 10;;

let iteratee = fun update_index index item ->
  if index = update_index
    then { text = item.text; checked = (not item.checked) }
    else item;;

let change_checked = fun state index ->
  {
    state with
      todo = List.mapi (iteratee index) state.todo;
  };;

let create_todo = fun state ->
  {
    value = "";
    todo = List.append state.todo [ { text = state.value; checked = false } ];
  };;

let remove_last_char = fun state ->
  {
    state with
      value = String.sub state.value 0 (String.length state.value - 1);
  };;

let append_char = fun state char ->
  {
    state with
      value = String.concat "" [ state.value; (String.make 1 char) ];
  };;

let handle_key = fun state key ->
  match key with
    | '\r' -> create_todo state;
    | '\b' -> remove_last_char state;
    | _ -> append_char state key;;

let rec render state =
  clear_graph ();
  List.iteri draw_item state.todo;
  draw_input state.value;
  let status = wait_next_event [ Button_down; Key_pressed ] in
    match status with
      | { keypressed = true } -> let next_state = handle_key state status.key in
        render next_state
      | { button = true } ->
        let next_state = change_checked state ((height - status.mouse_y) / item_height) in
        render next_state
      | _ -> render state;;

let initial_state = {
  value = "";
  todo = [
    { text = "foo"; checked = false };
    { text = "bar"; checked = true };
    { text = "baz"; checked = false };
  ];
};;

let _ =
  render initial_state;;
