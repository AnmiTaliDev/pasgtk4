# PasGTK4

Modern Pascal bindings for GTK4 with LibAdwaita support.

## Features

- **GTK4 Support**: Complete bindings for GTK4 widgets and functionality
- **LibAdwaita Integration**: Modern GNOME design with Adwaita components
- **Memory Safety**: Comprehensive error handling and nil checks
- **Cross-Platform**: Works on Linux, Windows, and macOS
- **Object-Oriented**: Clean Pascal classes wrapping GTK4 functionality
- **Conditional Debug**: Debug output controlled by compiler flags

## Requirements

### Linux
```bash
sudo apt install libgtk-4-dev libadwaita-1-dev
```

### Arch Linux
```bash
sudo pacman -S gtk4 libadwaita
```

**Or install directly from AUR:**
```bash
yay -S pasgtk4-git
```
Package: https://aur.archlinux.org/packages/pasgtk4-git

## Quick Start

### 1. Clone the repository
```bash
git clone https://github.com/AnmiTaliDev/pasgtk4.git
cd pasgtk4
```

### 2. Compile and run examples
```bash
meson setup builddir               
meson compile -C builddir

# Run with LibAdwaita
./example/example_main --adwaita
```

## Project Structure

```
pasgtk4/
src/
    wrapper.pas     # Low-level GTK4 bindings
    main.pas        # High-level Pascal classes
example/
    example_main.pas # Basic example application
README.md
```

## Basic Usage

```pascal
program simple_app;

uses
  SysUtils, main, wrapper;

var
  app: TModernExampleApp;

begin
  // Initialize PasGTK4
  if not InitializePasGTK4 then
    Halt(1);
  
  // Create and run modern application
  app := TModernExampleApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  // Cleanup
  FinalizePasGTK4;
end.
```

## API Overview

### High-Level Classes

- **TGTKApplication**: Base application class
- **TGTKSimpleWindow**: Window with vertical/horizontal layout
- **TGTKGridWindow**: Window with grid layout
- **TGTKModernWindow**: Modern GTK4 window with HeaderBar and PopoverMenu (recommended)
- **TGTKMenuWindow**: Compatibility mode window with traditional menus

### Core Functions

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

## Examples

The example application supports three modes:

### 1. Modern GTK4
```bash
./example/example_main
```
Features modern HeaderBar with PopoverMenu and GAction system.

### 2. LibAdwaita 
```bash
./example/example_main --adwaita  
```
Native GNOME design with LibAdwaita widgets.

### 3. Compatibility Mode
```bash
./example/example_main --compat
```
Shows traditional GTK3-style menu compatibility.

### Basic Window Code Example
```pascal
type
  TMyApp = class(TGTKSimpleWindow)
  public
    constructor Create;
    procedure SetupWindow; override;
  end;

constructor TMyApp.Create;
begin
  inherited Create('com.example.myapp');
  Title := 'My Application';
  Width := 400;
  Height := 300;
end;

procedure TMyApp.SetupWindow;
begin
  inherited SetupWindow;
  AddLabel('Welcome to PasGTK4!');
  AddButton('Click Me', @ButtonCallback, Self);
end;
```

### LibAdwaita Features
- Modern header bars
- Toast notifications
- Adaptive layouts
- System theme integration

## Debug Mode

Enable debug output during compilation:
```bash
fpc -dDEBUG -Fu./src example/example_main.pas
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## License

Licensed under the GNU Lesser General Public License v3.0. See LICENSE.md file for details.

## Author

AnmiTaliDev - 2025