program example_main;

{
 * PasGTK4 - Pascal bindings for GTK4
 * Example Application
 * Copyright (C) 2025 AnmiTaliDev
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
}

{$mode objfpc}{$H+}

uses
  SysUtils, main, wrapper;

type
  TDemoApp = class;
  TCompatApp = class;
  TAdwaitaApp = class;

{------------------------------------------------------------------------------}
{ Common Logic Procedures                                                      }
{------------------------------------------------------------------------------}
// These procedures contain the shared UI logic to avoid code duplication
// across different application classes (Modern, Compat, Adwaita).

procedure HandleButtonClick(StatusLabel: PGtkLabel; var ClickCount: Integer);
begin
  Inc(ClickCount);
  TPasGTK4.SetLabelText(StatusLabel, 'Clicked ' + IntToStr(ClickCount) + ' times!');
end;

procedure HandleFileNew(ContentEntry, StatusLabel: PGtkWidget);
begin
  TPasGTK4.SetEntryText(PGtkEntry(ContentEntry), '');
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'New file created');
end;

procedure HandleFileOpen(StatusLabel: PGtkWidget);
begin
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'File open dialog would appear');
end;

procedure HandleFileSave(ContentEntry, StatusLabel: PGtkWidget);
var
  text: string;
begin
  text := TPasGTK4.GetEntryText(PGtkEntry(ContentEntry));
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'Saved: ' + IntToStr(Length(text)) + ' characters');
end;

procedure HandleFileExit(StatusLabel: PGtkWidget);
begin
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'Goodbye!');
  // NOTE: Halt is used because a clean application exit function (like GApplication.Quit)
  // is not exposed in the current wrapper. In a real app, you would close the main window
  // or call the application quit method.
  Halt(0);
end;

procedure HandleEditCut(ContentEntry, StatusLabel: PGtkWidget);
var
  text: string;
begin
  text := TPasGTK4.GetEntryText(PGtkEntry(ContentEntry));
  TPasGTK4.SetEntryText(PGtkEntry(ContentEntry), '');
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'Cut: "' + text + '"');
end;

procedure HandleEditCopy(ContentEntry, StatusLabel: PGtkWidget);
var
  text: string;
begin
  text := TPasGTK4.GetEntryText(PGtkEntry(ContentEntry));
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'Copied: "' + text + '"');
end;

procedure HandleEditPaste(ContentEntry, StatusLabel: PGtkWidget);
begin
  TPasGTK4.SetEntryText(PGtkEntry(ContentEntry), 'Pasted text');
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'Text pasted');
end;

procedure HandleHelpAbout(StatusLabel: PGtkWidget);
begin
  TPasGTK4.SetLabelText(PGtkLabel(StatusLabel), 'PasGTK4 Demo - Modern Pascal GTK4 Bindings');
end;


{------------------------------------------------------------------------------}
{ TDemoApp - Modern GTK4 application with HeaderBar                            }
{------------------------------------------------------------------------------}
type
  TDemoApp = class(TGTKModernWindow)
  private
    FStatusLabel: PGtkLabel;
    FContentEntry: PGtkEntry;
    FClickCount: Integer;
  public
    constructor Create;
    procedure SetupWindow; override;

    // Action handlers
    procedure DoFileNew;   procedure DoFileOpen;  procedure DoFileSave;
    procedure DoFileExit;  procedure DoEditCut;   procedure DoEditCopy;
    procedure DoEditPaste; procedure DoHelpAbout; procedure DoButtonClick;
  end;

// cdecl callbacks that forward calls to the TDemoApp instance
procedure modern_callback_file_new(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoFileNew; end;
procedure modern_callback_file_open(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoFileOpen; end;
procedure modern_callback_file_save(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoFileSave; end;
procedure modern_callback_file_exit(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoFileExit; end;
procedure modern_callback_edit_cut(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoEditCut; end;
procedure modern_callback_edit_copy(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoEditCopy; end;
procedure modern_callback_edit_paste(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoEditPaste; end;
procedure modern_callback_help_about(a: Pointer; p: Pointer; d: Pointer); cdecl; begin TDemoApp(d).DoHelpAbout; end;
procedure modern_callback_button_click(w: PGtkWidget; d: Pointer); cdecl; begin TDemoApp(d).DoButtonClick; end;

constructor TDemoApp.Create;
begin
  inherited Create('com.anmitalidev.pasgtk4.demo');
  FClickCount := 0;
  Title := 'PasGTK4 Demo';
  Width := 600;
  Height := 400;
end;

procedure TDemoApp.SetupWindow;
var
  file_menu, edit_menu, help_menu: PGMenuModel;
begin
  inherited SetupWindow;

  // Create menus
  file_menu := CreateHeaderBarMenu('File');
  AddMenuAction(file_menu, 'New', 'file-new', @modern_callback_file_new, Self);
  AddMenuAction(file_menu, 'Open', 'file-open', @modern_callback_file_open, Self);
  AddMenuAction(file_menu, 'Save', 'file-save', @modern_callback_file_save, Self);
  AddMenuAction(file_menu, 'Exit', 'file-exit', @modern_callback_file_exit, Self);

  edit_menu := CreateHeaderBarMenu('Edit');
  AddMenuAction(edit_menu, 'Cut', 'edit-cut', @modern_callback_edit_cut, Self);
  AddMenuAction(edit_menu, 'Copy', 'edit-copy', @modern_callback_edit_copy, Self);
  AddMenuAction(edit_menu, 'Paste', 'edit-paste', @modern_callback_edit_paste, Self);

  help_menu := CreateHeaderBarMenu('Help');
  AddMenuAction(help_menu, 'About', 'help-about', @modern_callback_help_about, Self);

  // Add content
  AddLabel('Welcome to PasGTK4!');
  AddLabel('Modern GTK4 with HeaderBar and PopoverMenu');
  AddLabel('');
  AddButton('Click me!', @modern_callback_button_click, Self);
  AddLabel('');
  FContentEntry := AddEntry('Type something...');
  FStatusLabel := AddLabel('Ready');
  TPasGTK4.SetLabelJustify(FStatusLabel, GTK_JUSTIFY_CENTER);
end;

procedure TDemoApp.DoFileNew;   begin HandleFileNew(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoFileOpen;  begin HandleFileOpen(PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoFileSave;  begin HandleFileSave(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoFileExit;  begin HandleFileExit(PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoEditCut;   begin HandleEditCut(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoEditCopy;  begin HandleEditCopy(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoEditPaste; begin HandleEditPaste(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoHelpAbout; begin HandleHelpAbout(PGtkWidget(FStatusLabel)); end;
procedure TDemoApp.DoButtonClick; begin HandleButtonClick(FStatusLabel, FClickCount); end;


{------------------------------------------------------------------------------}
{ TCompatApp - Compatibility mode with traditional menus                       }
{------------------------------------------------------------------------------}
type
  TCompatApp = class(TGTKMenuWindow)
  private
    FStatusLabel: PGtkLabel;
    FContentEntry: PGtkEntry;
    FClickCount: Integer;
  public
    constructor Create;
    procedure SetupWindow; override;

    // Action handlers
    procedure DoFileNew;   procedure DoFileExit;
    procedure DoEditCopy;  procedure DoHelpAbout;
    procedure DoButtonClick;
  end;

// cdecl callbacks that forward calls to the TCompatApp instance
procedure compat_callback_file_new(w: PGtkWidget; d: Pointer); cdecl;   begin TCompatApp(d).DoFileNew; end;
procedure compat_callback_file_exit(w: PGtkWidget; d: Pointer); cdecl;  begin TCompatApp(d).DoFileExit; end;
procedure compat_callback_edit_copy(w: PGtkWidget; d: Pointer); cdecl;  begin TCompatApp(d).DoEditCopy; end;
procedure compat_callback_help_about(w: PGtkWidget; d: Pointer); cdecl; begin TCompatApp(d).DoHelpAbout; end;
procedure compat_callback_button_click(w: PGtkWidget; d: Pointer); cdecl; begin TCompatApp(d).DoButtonClick; end;

constructor TCompatApp.Create;
begin
  inherited Create('com.anmitalidev.pasgtk4.compat');
  FClickCount := 0;
  Title := 'PasGTK4 Compatibility Mode';
  Width := 500;
  Height := 350;
end;

procedure TCompatApp.SetupWindow;
var
  file_menu, edit_menu, help_menu: PGMenuModel;
begin
  inherited SetupWindow;

  // Create traditional menus (simulated with buttons as per library capability)
  file_menu := CreateMenu('File');
  AddWidget(AddMenuItem(file_menu, 'New', @compat_callback_file_new, Self));
  AddWidget(AddMenuItem(file_menu, 'Exit', @compat_callback_file_exit, Self));

  edit_menu := CreateMenu('Edit');
  AddWidget(AddMenuItem(edit_menu, 'Copy', @compat_callback_edit_copy, Self));

  help_menu := CreateMenu('Help');
  AddWidget(AddMenuItem(help_menu, 'About', @compat_callback_help_about, Self));

  // Add content
  AddLabel('GTK4 Compatibility Mode');
  AddLabel('');
  AddButton('Click me!', @compat_callback_button_click, Self);
  AddLabel('');
  FContentEntry := AddEntry('Type something...');
  FStatusLabel := AddLabel('Ready');
end;

procedure TCompatApp.DoFileNew;   begin HandleFileNew(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TCompatApp.DoFileExit;  begin HandleFileExit(PGtkWidget(FStatusLabel)); end;
procedure TCompatApp.DoEditCopy;  begin HandleEditCopy(PGtkWidget(FContentEntry), PGtkWidget(FStatusLabel)); end;
procedure TCompatApp.DoHelpAbout; begin HandleHelpAbout(PGtkWidget(FStatusLabel)); end;
procedure TCompatApp.DoButtonClick; begin HandleButtonClick(FStatusLabel, FClickCount); end;


{------------------------------------------------------------------------------}
{ TAdwaitaApp - LibAdwaita application                                         }
{------------------------------------------------------------------------------}
type
  TAdwaitaApp = class
  private
    FApp: PAdwApplication;
    FWindow: PAdwApplicationWindow;
    FStatusLabel: PGtkLabel;
    FClickCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Run: Integer;
    procedure OnActivate(app: PAdwApplication; data: Pointer);
    procedure DoButtonClick;
  end;

// cdecl callbacks that forward calls to the TAdwaitaApp instance
procedure adwaita_callback_activate(app: PAdwApplication; d: Pointer); cdecl; begin TAdwaitaApp(d).OnActivate(app, d); end;
procedure adwaita_callback_button_click(w: PGtkWidget; d: Pointer); cdecl; begin TAdwaitaApp(d).DoButtonClick; end;

constructor TAdwaitaApp.Create;
begin
  inherited Create;
  FApp := TPasGTK4.CreateAdwApplication('com.anmitalidev.pasgtk4.adwaita');
  FClickCount := 0;
  TPasGTK4.ConnectApplicationSignal(PGtkApplication(FApp), 'activate', @adwaita_callback_activate, Self);
end;

destructor TAdwaitaApp.Destroy;
begin
  inherited Destroy;
end;

procedure TAdwaitaApp.OnActivate(app: PAdwApplication; data: Pointer);
var
  header_bar: PAdwHeaderBar;
  title_label: PGtkLabel;
  button: PGtkButton;
  main_box: PGtkBox;
begin
  FWindow := TPasGTK4.CreateAdwWindow(FApp);
  TPasGTK4.SetWindowSize(PGtkWindow(FWindow), 500, 300);

  // Create HeaderBar
  header_bar := TPasGTK4.CreateAdwHeaderBar;
  title_label := TPasGTK4.CreateLabel('PasGTK4 + LibAdwaita');
  TPasGTK4.SetAdwHeaderBarTitle(header_bar, PGtkWidget(title_label));

  // Create main content
  main_box := TPasGTK4.CreateVerticalBox(12);
  TPasGTK4.SetWidgetMargins(PGtkWidget(main_box), 24, 24, 24, 24);

  TPasGTK4.AddToBox(main_box, PGtkWidget(TPasGTK4.CreateLabel('Welcome to LibAdwaita!')));
  button := TPasGTK4.CreateButton('Click me!');
  TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @adwaita_callback_button_click, Self);
  TPasGTK4.AddToBox(main_box, PGtkWidget(button));
  FStatusLabel := TPasGTK4.CreateLabel('Ready');
  TPasGTK4.AddToBox(main_box, PGtkWidget(FStatusLabel));

  TPasGTK4.SetAdwWindowContent(FWindow, PGtkWidget(main_box));
  TPasGTK4.ShowWindow(PGtkWindow(FWindow));
end;

function TAdwaitaApp.Run: Integer;
begin
  Result := TPasGTK4.RunApplication(PGtkApplication(FApp));
end;

procedure TAdwaitaApp.DoButtonClick;
begin
  HandleButtonClick(FStatusLabel, FClickCount);
end;


{------------------------------------------------------------------------------}
{ Main Program                                                                 }
{------------------------------------------------------------------------------}
var
  app: TObject;
  result_code: Integer;
  use_compat, use_adwaita: Boolean;

begin
  WriteLn('PasGTK4 Example Application');
  WriteLn('Version: ', GetPasGTK4Version);
  WriteLn('---');

  // Check mode
  use_compat := (ParamCount > 0) and (ParamStr(1) = '--compat');
  use_adwaita := (ParamCount > 0) and (ParamStr(1) = '--adwaita');

  app := nil;
  result_code := 0;

  try
    if use_compat then
    begin
      WriteLn('Mode: Compatibility GTK4');
      if not InitializePasGTK4 then Halt(1);
      app := TCompatApp.Create;
      result_code := TCompatApp(app).Run;
    end
    else if use_adwaita then
    begin
      WriteLn('Mode: LibAdwaita');
      if not InitializePasGTK4WithAdwaita then Halt(1);
      app := TAdwaitaApp.Create;
      result_code := TAdwaitaApp(app).Run;
    end
    else
    begin
      WriteLn('Mode: Modern GTK4 HeaderBar');
      if not InitializePasGTK4 then Halt(1);
      app := TDemoApp.Create;
      result_code := TDemoApp(app).Run;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      result_code := 1;
    end;
  end;

  if app <> nil then
    app.Free;

  FinalizePasGTK4;
  Halt(result_code);
end.
