# API Reference

## TPasGTK4 - Main Bindings Class

### Initialization and Finalization

#### `Initialize(): Boolean`
Initializes the GTK4 library.
- **Returns**: `True` on success, `False` on error

#### `InitializeAdwaita(): Boolean`
Initializes LibAdwaita (requires prior GTK4 initialization).
- **Returns**: `True` on success, `False` on error

#### `Finalize()`
Frees resources and unloads libraries.

#### `IsInitialized(): Boolean`
Checks if GTK4 is initialized.

#### `IsAdwaitaInitialized(): Boolean`
Checks if LibAdwaita is initialized.

### Applications

#### `CreateApplication(app_id: string): PGtkApplication`
Creates a GTK4 application.
- **app_id**: Application identifier (e.g., 'com.example.myapp')
- **Returns**: Pointer to application or `nil` on error

#### `RunApplication(app: PGtkApplication): Integer`
Runs the main application loop.
- **Returns**: Application exit code

#### `ConnectApplicationSignal(app, signal, callback, data)`
Connects a signal handler to the application.

### Windows

#### `CreateWindow(app: PGtkApplication): PGtkWindow`
Creates an application window.

#### `SetWindowTitle(window: PGtkWindow; title: string)`
Sets the window title.

#### `SetWindowSize(window: PGtkWindow; width, height: Integer)`
Sets the default window size.

#### `ShowWindow(window: PGtkWindow)`
Shows the window.

#### `SetWindowChild(window: PGtkWindow; child: PGtkWidget)`
Sets the window's child widget.

### Widgets

#### `CreateButton(text: string): PGtkButton`
Creates a button with text.

#### `CreateLabel(text: string): PGtkLabel`
Creates a label with text.

#### `SetLabelText(label: PGtkLabel; text: string)`
Changes the label text.

#### `SetLabelJustify(label: PGtkLabel; justify: GtkJustification)`
Sets the label text alignment.

#### `CreateEntry(): PGtkEntry`
Creates a text entry field.

#### `SetEntryText(entry: PGtkEntry; text: string)`
Sets text in the entry field.

#### `GetEntryText(entry: PGtkEntry): string`
Gets text from the entry field.

#### `SetEntryPlaceholder(entry: PGtkEntry; text: string)`
Sets placeholder text.

### Containers

#### `CreateBox(orientation: GtkOrientation; spacing: Integer): PGtkBox`
Creates a box container.
- **orientation**: `GTK_ORIENTATION_HORIZONTAL` or `GTK_ORIENTATION_VERTICAL`
- **spacing**: Space between elements

#### `CreateVerticalBox(spacing: Integer): PGtkBox`
Creates a vertical box.

#### `CreateHorizontalBox(spacing: Integer): PGtkBox`
Creates a horizontal box.

#### `AddToBox(box: PGtkBox; widget: PGtkWidget)`
Adds widget to the end of the box.

#### `PrependToBox(box: PGtkBox; widget: PGtkWidget)`
Adds widget to the beginning of the box.

#### `CreateGrid(): PGtkGrid`
Creates a grid container.

#### `AttachToGrid(grid, widget, left, top, width, height)`
Places widget in grid at specified coordinates and size.

### Alignment and Margins

#### `SetWidgetAlign(widget: PGtkWidget; halign, valign: GtkAlign)`
Sets horizontal and vertical alignment of widget.

Alignment constants:
- `GTK_ALIGN_FILL` - fill available space
- `GTK_ALIGN_START` - align to start
- `GTK_ALIGN_END` - align to end
- `GTK_ALIGN_CENTER` - center
- `GTK_ALIGN_BASELINE` - align to baseline

#### `SetWidgetMargins(widget, top, bottom, start, end_margin)`
Sets widget margins.

### Signals

#### `ConnectSignal(widget, signal, callback, data)`
Connects a signal handler to a widget.

Common signals:
- `'clicked'` - for buttons
- `'changed'` - for entry fields
- `'activate'` - for activation

### LibAdwaita Methods

#### `CreateAdwApplication(app_id: string): PAdwApplication`
Creates an Adwaita application.

#### `CreateAdwWindow(app: PAdwApplication): PAdwApplicationWindow`
Creates an Adwaita window.

#### `SetAdwWindowContent(window, content)`
Sets Adwaita window content.

#### `CreateHeaderBar(): PAdwHeaderBar`
Creates a header bar.

#### `CreateToastOverlay(): PAdwToastOverlay`
Creates an overlay for toast notifications.

#### `CreateToast(title: string): PAdwToast`
Creates a toast notification.

#### `ShowToast(overlay: PAdwToastOverlay; toast: PAdwToast)`
Shows a toast notification.

## High-Level Classes

### TGTKApplication

Base class for GTK4 applications.

#### Properties
- `Title: string` - window title
- `Width, Height: Integer` - window dimensions
- `OnActivate: TGtkApplicationCallback` - activation handler

#### Methods
- `constructor Create(app_id: string)` - create application
- `Run(): Integer` - run application
- `SetupWindow()` - virtual window setup method
- `GetWindow(): PGtkWindow` - get window pointer
- `GetApplication(): PGtkApplication` - get application pointer

### TGTKSimpleWindow

Simple window with linear widget layout.

#### Constructor
```pascal
Create(app_id: string; vertical: Boolean = True; spacing: Integer = 10)
```

#### Properties
- `MainBox: PGtkBox` - main container
- `IsVertical: Boolean` - vertical layout
- `Spacing: Integer` - spacing between widgets

#### Methods
- `AddWidget(widget: PGtkWidget)` - add widget
- `AddButton(text, callback, data): PGtkButton` - add button
- `AddLabel(text): PGtkLabel` - add label
- `AddEntry(placeholder): PGtkEntry` - add entry field

### TGTKGridWindow

Window with grid widget layout.

#### Properties
- `MainGrid: PGtkGrid` - main grid

#### Methods
- `AttachWidget(widget, left, top, w, h)` - place widget
- `AttachButton(text, left, top, w, h, callback, data): PGtkButton`
- `AttachLabel(text, left, top, w, h): PGtkLabel`
- `AttachEntry(left, top, w, h, placeholder): PGtkEntry`

## Utilities

### `InitializePasGTK4(): Boolean`
Initializes GTK4 only.

### `InitializePasGTK4WithAdwaita(): Boolean`
Initializes GTK4 and LibAdwaita.

### `FinalizePasGTK4()`
Cleans up all resources.

### `GetPasGTK4Version(): string`
Returns library version.