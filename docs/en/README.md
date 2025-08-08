# PasGTK4 - Pascal Bindings for GTK4

Modern Pascal bindings for GTK4 with LibAdwaita support.

## Features

- **GTK4 Support**: Complete bindings for GTK4 widgets and functionality
- **LibAdwaita Integration**: Modern GNOME design with Adwaita components
- **Memory Safety**: Comprehensive error handling and nil checks
- **Cross-Platform**: Works on Linux, Windows, and macOS
- **Object-Oriented**: Clean Pascal classes wrapping GTK4 functionality
- **Conditional Debug**: Debug output controlled by compiler flags

## Quick Start

### Dependencies

#### Ubuntu/Debian
```bash
sudo apt install libgtk-4-dev libadwaita-1-dev
```

#### Arch Linux
```bash
sudo pacman -S gtk4 libadwaita
```

**Or install directly from AUR:**
```bash
yay -S pasgtk4-git
```
Package: https://aur.archlinux.org/packages/pasgtk4-git

### Build and Run

```bash
# Build with Meson
meson setup builddir
meson compile -C builddir

# Run with LibAdwaita
./example/example_main --adwaita

# Or compile directly
fpc -Fu./src example/example_main.pas -oexample/example_main
./example/example_main
```

## Project Structure

```
pasgtk4/
├── docs/
│   ├── ru/          # Russian documentation
│   └── en/          # English documentation
├── src/
│   ├── wrapper.pas  # Low-level GTK4 bindings
│   └── main.pas     # High-level Pascal classes
├── example/
│   └── example_main.pas  # Basic example application
└── README.md
```

## Architecture

### Low Level (wrapper.pas)
- Dynamic loading of GTK4/LibAdwaita libraries
- Direct C function wrappers
- Memory and error management
- GTK4 data types

### High Level (main.pas)
- Object-oriented classes
- Simplified API for rapid development
- Automatic resource management
- Built-in event handling

## Basic Usage

```pascal
program simple_app;

uses
  SysUtils, main, wrapper;

var
  app: TExampleApp;

begin
  // Initialize PasGTK4 with LibAdwaita
  if not InitializePasGTK4WithAdwaita then
    Halt(1);
  
  // Create and run application
  app := TExampleApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  // Cleanup resources
  FinalizePasGTK4;
end.
```

## High-Level Classes

### TGTKApplication
Base application class with core functionality:
- Application lifecycle management
- Main window creation and setup
- Activation event handling

### TGTKSimpleWindow
Simple window with vertical or horizontal layout:
- Automatic container management
- Simple widget addition methods
- Built-in margin handling

### TGTKGridWindow
Window with grid layout for complex interfaces:
- Precise widget positioning
- Multi-column layout support
- Flexible size management

## Core Functions

### Initialization
- `InitializePasGTK4()`: Initialize GTK4 only
- `InitializePasGTK4WithAdwaita()`: Initialize GTK4 + LibAdwaita
- `FinalizePasGTK4()`: Cleanup resources

### Widget Creation
```pascal
// Create widgets
button := TPasGTK4.CreateButton('Click me');
label := TPasGTK4.CreateLabel('Hello World');
entry := TPasGTK4.CreateEntry;

// Set properties
TPasGTK4.SetEntryText(entry, 'Initial text');
TPasGTK4.SetLabelText(label, 'New text');

// Connect signals
TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @callback, data);
```

## Debug Mode

Enable debug output during compilation:
```bash
fpc -dDEBUG -Fu./src example/example_main.pas
```

## Examples

See the `example/` folder for complete examples demonstrating various widgets and library capabilities.

## License

Licensed under the GNU Lesser General Public License v3.0. See LICENSE.md file for details.