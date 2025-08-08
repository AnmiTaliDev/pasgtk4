# AUR Installation Guide

## Installing pasgtk4-git from AUR

### Using yay (recommended)

```bash
yay -S pasgtk4-git
```

### Using paru

```bash
paru -S pasgtk4-git
```

### Manual installation

1. Clone the AUR package:
```bash
git clone https://aur.archlinux.org/pasgtk4-git.git
cd pasgtk4-git
```

2. Build and install:
```bash
makepkg -si
```

## Dependencies

The following packages will be automatically installed:
- `gtk4` - GTK4 library
- `libadwaita` - Modern GNOME library
- `fpc` - Free Pascal Compiler (optional, for development)

## Package Contents

After installation, the following will be available:

### Headers and Sources
- `/usr/include/pasgtk4/` - Pascal source files
- `/usr/lib/pkgconfig/pasgtk4.pc` - pkg-config file

### Documentation
- `/usr/share/doc/pasgtk4/README.md` - Main documentation
- `/usr/share/doc/pasgtk4/LICENSE.md` - License information
- `/usr/share/doc/pasgtk4/ru/` - Russian documentation
- `/usr/share/doc/pasgtk4/en/` - English documentation

### Examples
- `/usr/share/pasgtk4/examples/` - Example Pascal programs
- `/usr/share/pasgtk4/meson.build` - Meson build file

## Using PasGTK4

After installation, you can use PasGTK4 in your Pascal projects:

### Compile with FPC

```bash
fpc -Fu/usr/include/pasgtk4 your_program.pas
```

### Using with Meson

Create a `meson.build` file:
```meson
project('myapp', 'pascal')

pasgtk4_dep = dependency('pasgtk4')

executable('myapp',
  'main.pas',
  dependencies: [pasgtk4_dep]
)
```

### Example Usage

```pascal
program hello_world;

uses
  SysUtils, main, wrapper;

type
  TMyApp = class(TGTKSimpleWindow)
  public
    constructor Create;
    procedure SetupWindow; override;
  end;

constructor TMyApp.Create;
begin
  inherited Create('com.example.hello');
  Title := 'Hello PasGTK4';
end;

procedure TMyApp.SetupWindow;
begin
  inherited SetupWindow;
  AddLabel('Hello, World from PasGTK4!');
end;

var
  app: TMyApp;

begin
  if not InitializePasGTK4WithAdwaita then
    Halt(1);
    
  app := TMyApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  FinalizePasGTK4;
end.
```

## Uninstalling

To remove the package:

```bash
sudo pacman -R pasgtk4-git
```

## Troubleshooting

### Build fails with "fpc not found"
Install the Free Pascal Compiler:
```bash
sudo pacman -S fpc
```

### Missing GTK4 development files
Install GTK4 development packages:
```bash
sudo pacman -S gtk4 libadwaita
```

### Permission errors
Make sure you have proper permissions or run with sudo:
```bash
sudo makepkg -si
```