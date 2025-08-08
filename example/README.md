# PasGTK4 Example Application

A clean, comprehensive example demonstrating all PasGTK4 features.

## Building

```bash
# From project root
fpc -B example/example_main.pas -Fu./src -oexample/example_main
```

## Running

### Modern GTK4 (Default)
```bash
./example/example_main
```
- Modern HeaderBar with PopoverMenu
- GAction/GActionGroup system
- Clean modern GNOME design
- **Recommended for new applications**

### LibAdwaita Mode
```bash
./example/example_main --adwaita
```
- Native GNOME styling
- LibAdwaita widgets
- System theme integration

### Compatibility Mode
```bash
./example/example_main --compat
```
- Traditional GTK3-style menus
- Compatibility warnings
- For legacy applications

## Code Structure

### Classes

- **TDemoApp** - Modern GTK4 with HeaderBar (default)
- **TAdwaitaApp** - LibAdwaita application  
- **TCompatApp** - Compatibility mode application

### Features Demonstrated

- Widget creation and management
- Menu systems (modern and traditional)
- Event handling with callbacks
- GAction system for modern apps
- Error handling and initialization
- Clean Pascal OOP patterns

## Requirements

- GTK4 development libraries
- LibAdwaita (for --adwaita mode)
- Free Pascal Compiler