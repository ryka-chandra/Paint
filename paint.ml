(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; p1: point; p2: point; thickness: int}
  | Points of { color: Gctx.color; points: point list; thickness: int}
  | Ellipse of {color: color; p1: point; p2: point; thickness: int}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode 
  | EllipseStartMode
  | EllipseEndMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;
  mutable preview : shape option;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  mutable thickness : int
  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *) 
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = 1
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 *)
  
}



(** This function creates a graphics context with the appropriate
    pen color. *)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (thickness: int) : gctx =
  let g = with_color g c in
  with_thickness g thickness


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points (with_params g p.color p.thickness) p.points
      | Ellipse e -> 
        let x1 = fst e.p1 in
        let x2 = fst e.p2 in
        let y1 = snd e.p1 in 
        let y2 = snd e.p2 in
        let rad_x = (x1 - x2) / 2 in
        let rad_y = (y1 - y2) / 2 in
        draw_ellipse (with_params g e.color e.thickness)
          (x2 + rad_x, y2 + rad_y) (abs rad_x) (abs rad_y)
    end in
  Deque.iterate draw_shape paint.shapes;
  begin match paint.preview with
  | None -> ()
  | Some s -> draw_shape s
  end

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
            ()
          | PointMode -> 
            paint.preview
              <- Some (Points { color=paint.color; points=[p];
                  thickness=paint.thickness})
          | EllipseStartMode -> paint.mode <- EllipseEndMode p
          | EllipseEndMode p1 -> ()
       end)
    | MouseDrag ->
      begin match paint.mode with
      | LineStartMode -> ()
      | LineEndMode p1 ->
        paint.preview <- Some (Line {color=paint.color; p1=p1; p2=p;
          thickness = paint.thickness})
      | PointMode -> paint.preview <- 
        begin match paint.preview with
        | None -> None
        | Some (Points x) -> Some (
          Points {color=paint.color; points=p :: x.points;
            thickness = paint.thickness})
        | _ -> None
        end
      | EllipseStartMode -> ()
      | EllipseEndMode p1 -> 
        paint.preview <- Some (Ellipse {color=paint.color; p1=p1; p2=p;
          thickness = paint.thickness})
      end
    | MouseUp ->
      begin match paint.mode with
      | LineStartMode -> ()
      | LineEndMode p1 ->
        Deque.insert_tail
          (Line {color=paint.color; p1=p1; p2=p;
            thickness = paint.thickness}) paint.shapes;
        paint.mode <- LineStartMode;
        paint.preview <- None
      | PointMode ->   
        begin match paint.preview with
        | None -> ()
        | Some s -> Deque.insert_tail s paint.shapes
        end;
        paint.preview <- None
      | EllipseStartMode -> () 
      | EllipseEndMode p1 -> 
        Deque.insert_tail
          (Ellipse {color=paint.color; p1=p1; p2=p;
            thickness = paint.thickness}) paint.shapes;
        paint.mode <- EllipseStartMode;
        paint.preview <- None
      end
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 is to
   make improvements to make the layout more appealing. You may choose
   to arrange the buttons and other GUI elements of the paint program
   however you like (so long as it is easily apparent how to use the
   interface).  The sample screenshot of our solution shows one
   possible design.  Also, feel free to improve the visual components
   of the GUI; for example, our solution puts borders around the
   buttons and uses a custom "color button" that changes its
   appearance based on whether or not the color is currently
   selected. *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"
let (w_line, lc_line, nc_line) = button "line"
let (w_point, lc_point, nc_point) = button "point"
let (w_ellipse, lc_ellipse, nc_ellipse) = button "ellipse"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 3 and 4, and potentially 2
   (depending on your implementation). *)

let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)

let func_line () : unit =
  paint.mode <- LineStartMode

;; nc_line.add_event_listener (mouseclick_listener func_line)

let func_point () : unit =
  paint.mode <- PointMode

;; nc_point.add_event_listener (mouseclick_listener func_point)

let func_ellipse () : unit = 
  paint.mode <- EllipseStartMode

;; nc_ellipse.add_event_listener (mouseclick_listener func_ellipse)

(** A spacer widget *)
let spacer : widget = space (10,10)

let (w_thick, vc_thick) = checkbox false "thick"

;; vc_thick.add_change_listener (fun x -> if x then paint.thickness <- 10
                                else paint.thickness <- 1)

let (w_slider, vc_slider) = slider 1

let (label_slider, _) = label "redness"

;; vc_slider.add_change_listener (fun x -> 
                                  let r = x * 256 / 100 in
                                  paint.color <- {paint.color with r = r})

(** The mode toolbar, initially containing just the Undo button.
    TODO: you will need to modify this widget to add more buttons
    to the toolbar in Task 1, Tasks 5, and possibly 6. *)
let mode_toolbar : widget = hlist [border w_undo; spacer;
  border w_line; spacer; border w_point; spacer; border w_ellipse; spacer;
  w_thick; spacer; border (hpair label_slider w_slider)]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)
let color_list = [black; white; red; green; blue; yellow; cyan; magenta]

let color_buttons = List.map (fun x -> color_button x) color_list

let spaced_color_buttons = List.fold_right (fun x acc -> x :: spacer :: acc)
  color_buttons []

let color_toolbar : widget =
  hlist spaced_color_buttons

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
  vlist [paint_canvas; spacer; mode_toolbar; spacer; color_toolbar]


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
