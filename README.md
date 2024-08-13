# Paint

This project is a paint application implemented in OCaml, structured into three layers: the painting logic, the drawing functions, and a collection of widgets that facilitate the creation and management of the user interface. The application allows users to draw shapes on a canvas, select colors, and manage their drawings through a simple graphical user interface (GUI).

## Widgets

### What Are Widgets?

Widgets are the building blocks of the paint application's user interface. They represent all visible components, such as buttons and the canvas itself, and even the entire application as a single widget composed of multiple other widgets. Widgets simplify the assembly of user interfaces by encapsulating functionality and user interactions.

### How Do Widgets Work?

A widget in this application is a record with three key functions:

1. **Repaint**: Defines how the widget draws itself.
2. **Handle**: Manages events that occur within the widget's region of the screen (e.g., mouse clicks).
3. **Size**: Determines the widget's size at any given moment.

These functions make widgets versatile and adaptable, allowing the application to offer a wide range of features.

## Application Structure

### Understanding `paint.ml`

The main application logic is handled in `paint.ml`. Here’s how it works:

- **State Management**: The application uses a mutable record, `state`, to store its current state. This includes:
  - `state.shapes`: A sequence of shapes drawn by the user.
  - `state.mode`: The current input mode of the paint program (e.g., drawing a line).
  - `state.color`: The currently selected pen color.

- **GUI Components**: The main GUI consists of three primary components:
  - `mode_toolbar`: A toolbar for selecting the drawing mode.
  - `color_toolbar`: A toolbar for selecting the pen color.
  - `paint_canvas`: The canvas where shapes are drawn.

### Shapes

- **Drawing Shapes**: The initial version of the application supports drawing lines. A line is represented by the `Line` constructor, which stores its color, start point (`p1`), and end point (`p2`).

- **Shape Storage**: The shapes are stored in `state.shapes`, a `Deque.deque` that records the sequence of shapes in the order they were drawn. This allows for both ordered drawing and undo functionality.

### Paint Canvas

- **Repainting**: The `repaint` function is responsible for updating the canvas. It iterates over the deque of shapes, drawing each one in the order they were added.

- **Event Handling**: Mouse clicks on the canvas are handled by the `paint_action` function, which listens for events via `paint_canvas_controller`. The application supports two drawing modes: `LineStartMode` and `LineEndMode`. The first click sets the start point of the line, and the second click sets the end point.

### Toolbars and Layout

- **Undo Functionality**: Implemented in the `Paint.undo` function, allowing users to remove the most recently drawn shape.

- **Color Selection**: Color buttons are created using the `Paint.color_button` function, which sets `state.color` based on the user's selection. The current color is displayed using a `color_indicator` widget.

- **Layout**: The toolbars and canvas are arranged using `Widget.hpair` layouts. The entire application is then run by executing the event loop on the top-level layout widget.

## Future Enhancements

- **Additional Shapes**: Extend the application to support drawing other shapes like rectangles, circles, etc.
- **Enhanced GUI**: Improve the user interface by adding more features and refining the existing layout.
- **Save/Load Functionality**: Allow users to save their drawings and load them later.

## Conclusion

This paint application is a versatile tool for creating simple drawings in OCaml. By leveraging widgets and a well-structured application state, it provides a solid foundation for further development and enhancements.
