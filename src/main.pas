unit main;

{*
 * PasGTK4 - Pascal bindings for GTK4
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
 *}

{$mode objfpc}{$H+}

interface

uses
  wrapper, SysUtils;

type
  { TGTKApplication - High-level class for creating GTK4 applications }
  TGTKApplication = class
  private
    FApp: PGtkApplication;
    FWindow: PGtkWindow;
    FTitle: string;
    FWidth, FHeight: Integer;
    FOnActivate: TGtkApplicationCallback;
    
  protected
    procedure DefaultActivate(app: PGtkApplication; data: Pointer); virtual;
    
  public
    constructor Create(const app_id: string);
    destructor Destroy; override;
    
    // Properties
    property Title: string read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property OnActivate: TGtkApplicationCallback read FOnActivate write FOnActivate;
    
    // Methods
    function Run: Integer;
    procedure SetupWindow; virtual;
    function GetWindow: PGtkWindow;
    function GetApplication: PGtkApplication;
  end;

  { TGTKSimpleWindow - Simple window with basic functionality }
  TGTKSimpleWindow = class(TGTKApplication)
  private
    FMainBox: PGtkBox;
    FIsVertical: Boolean;
    FSpacing: Integer;
    
  public
    constructor Create(const app_id: string; vertical: Boolean = True; spacing: Integer = 10);
    
    property MainBox: PGtkBox read FMainBox;
    property IsVertical: Boolean read FIsVertical write FIsVertical;
    property Spacing: Integer read FSpacing write FSpacing;
    
    procedure SetupWindow; override;
    procedure AddWidget(widget: PGtkWidget);
    function AddButton(const text: string; callback: TGtkCallback; data: Pointer = nil): PGtkButton;
    function AddLabel(const text: string): PGtkLabel;
    function AddEntry(const placeholder: string = ''): PGtkEntry;
  end;

  { TGTKGridWindow - Window with grid layout for complex layouts }
  TGTKGridWindow = class(TGTKApplication)
  private
    FMainGrid: PGtkGrid;
    
  public
    constructor Create(const app_id: string);
    
    property MainGrid: PGtkGrid read FMainGrid;
    
    procedure SetupWindow; override;
    procedure AttachWidget(widget: PGtkWidget; left, top, w, h: Integer);
    function AttachButton(const text: string; left, top, w, h: Integer; callback: TGtkCallback; data: Pointer = nil): PGtkButton;
    function AttachLabel(const text: string; left, top, w, h: Integer): PGtkLabel;
    function AttachEntry(left, top, w, h: Integer; const placeholder: string = ''): PGtkEntry;
  end;

  { TGTKMenuWindow - Window with GTK4-style menu support }
  TGTKMenuWindow = class(TGTKSimpleWindow)
  private
    FMenuBar: PGtkWidget; // Just a label for now, GTK4 doesn't have traditional menu bars
    FMenus: array of PGMenuModel;
    FMenuCount: Integer;
    FWarningShown: Boolean;
    
  public
    constructor Create(const app_id: string; vertical: Boolean = True; widget_spacing: Integer = 10);
    
    property MenuBar: PGtkWidget read FMenuBar;
    
    procedure SetupWindow; override;
    function CreateMenu(const menu_title: string): PGMenuModel;
    function AddMenuItem(menu: PGMenuModel; const text: string; callback: TGtkCallback = nil; data: Pointer = nil): PGtkWidget;
    function AddMenuItemWithMnemonic(menu: PGMenuModel; const text: string; callback: TGtkCallback = nil; data: Pointer = nil): PGtkWidget;
    function AddCheckMenuItem(menu: PGMenuModel; const text: string; callback: TGtkCallback = nil; data: Pointer = nil): PGtkWidget;
    function AddRadioMenuItem(menu: PGMenuModel; const text: string; group: Pointer = nil; callback: TGtkCallback = nil; data: Pointer = nil): PGtkWidget;
    procedure AddSeparatorMenuItem(menu: PGMenuModel);
    function CreateSubmenu(parent_menu: PGMenuModel; const submenu_title: string): PGMenuModel;
  end;

  { TGTKModernWindow - Modern GTK4 window with HeaderBar and PopoverMenu }
  TGTKModernWindow = class(TGTKApplication)
  private
    FHeaderBar: PGtkHeaderBar;
    FMainBox: PGtkBox;
    FActionGroup: PGSimpleActionGroup;
    FMenus: array of record
      Button: PGtkMenuButton;
      Model: PGMenuModel;
      MenuTitle: string;
    end;
    FMenuCount: Integer;
    
  public
    constructor Create(const app_id: string);
    
    property HeaderBar: PGtkHeaderBar read FHeaderBar;
    property MainBox: PGtkBox read FMainBox;
    
    procedure SetupWindow; override;
    procedure AddHeaderBarButton(button: PGtkWidget; pack_start: Boolean = True);
    function CreateHeaderBarMenu(const menu_title: string): PGMenuModel;
    function AddMenuAction(menu: PGMenuModel; const label_text, action_name: string; callback: TGActionCallback = nil; data: Pointer = nil): Boolean;
    procedure AddWidget(widget: PGtkWidget);
    function AddButton(const text: string; callback: TGtkCallback; data: Pointer = nil): PGtkButton;
    function AddLabel(const text: string): PGtkLabel;
    function AddEntry(const placeholder: string = ''): PGtkEntry;
  end;

// Utilities
function InitializePasGTK4: Boolean;
function InitializePasGTK4WithAdwaita: Boolean;
procedure FinalizePasGTK4;
function GetPasGTK4Version: string;

implementation

// Utilities
function InitializePasGTK4: Boolean;
begin
  Result := TPasGTK4.Initialize;
end;

function InitializePasGTK4WithAdwaita: Boolean;
begin
  Result := TPasGTK4.Initialize;
  if Result then
    Result := TPasGTK4.InitializeAdwaita;
end;

procedure FinalizePasGTK4;
begin
  TPasGTK4.Finalize;
end;

function GetPasGTK4Version: string;
begin
  Result := TPasGTK4.GetVersion;
end;

// Wrapper procedure for calling class method
procedure application_activate_wrapper(app: PGtkApplication; data: Pointer); cdecl;
var
  gtk_app: TGTKApplication;
begin
  if data <> nil then
  begin
    gtk_app := TGTKApplication(data);
    gtk_app.DefaultActivate(app, data);
  end;
end;

{ TGTKApplication }

constructor TGTKApplication.Create(const app_id: string);
begin
  inherited Create;
  
  if not TPasGTK4.IsInitialized then
    raise Exception.Create('PasGTK4 not initialized. Call InitializePasGTK4 before creating application.');
  
  FApp := TPasGTK4.CreateApplication(app_id);
  if FApp = nil then
    raise Exception.Create('Failed to create GTK application');
  
  FWindow := nil;
  FTitle := 'PasGTK4 Application';
  FWidth := 800;
  FHeight := 600;
  FOnActivate := nil;
  
  // Connect activation handler
  TPasGTK4.ConnectApplicationSignal(FApp, 'activate', @application_activate_wrapper, Self);
end;

destructor TGTKApplication.Destroy;
begin
  // GTK objects are managed by GTK library itself
  inherited Destroy;
end;

procedure TGTKApplication.DefaultActivate(app: PGtkApplication; data: Pointer);
begin
  // Create window
  FWindow := TPasGTK4.CreateWindow(FApp);
  TPasGTK4.SetWindowTitle(FWindow, FTitle);
  TPasGTK4.SetWindowSize(FWindow, FWidth, FHeight);
  
  // Call user handler or basic setup
  if Assigned(FOnActivate) then
    FOnActivate(app, data)
  else
    SetupWindow;
  
  // Show window
  TPasGTK4.ShowWindow(FWindow);
end;

function TGTKApplication.Run: Integer;
begin
  Result := TPasGTK4.RunApplication(FApp);
end;

procedure TGTKApplication.SetupWindow;
begin
  // Basic implementation - empty window
  // Overridden in descendants
end;

function TGTKApplication.GetWindow: PGtkWindow;
begin
  Result := FWindow;
end;

function TGTKApplication.GetApplication: PGtkApplication;
begin
  Result := FApp;
end;

{ TGTKSimpleWindow }

constructor TGTKSimpleWindow.Create(const app_id: string; vertical: Boolean; spacing: Integer);
begin
  inherited Create(app_id);
  FIsVertical := vertical;
  FSpacing := spacing;
  FMainBox := nil;
end;

procedure TGTKSimpleWindow.SetupWindow;
begin
  // Create main container
  if FIsVertical then
    FMainBox := TPasGTK4.CreateVerticalBox(FSpacing)
  else
    FMainBox := TPasGTK4.CreateHorizontalBox(FSpacing);
  
  // Set margins
  TPasGTK4.SetWidgetMargins(PGtkWidget(FMainBox), 10, 10, 10, 10);
  
  // Add to window
  TPasGTK4.SetWindowChild(FWindow, PGtkWidget(FMainBox));
end;

procedure TGTKSimpleWindow.AddWidget(widget: PGtkWidget);
begin
  if FMainBox = nil then
  begin
    WriteLn('Window not initialized');
    Exit;
  end;
  TPasGTK4.AddToBox(FMainBox, widget);
end;

function TGTKSimpleWindow.AddButton(const text: string; callback: TGtkCallback; data: Pointer): PGtkButton;
begin
  Result := TPasGTK4.CreateButton(text);
  if Assigned(callback) then
    TPasGTK4.ConnectSignal(PGtkWidget(Result), 'clicked', callback, data);
  AddWidget(PGtkWidget(Result));
end;

function TGTKSimpleWindow.AddLabel(const text: string): PGtkLabel;
begin
  Result := TPasGTK4.CreateLabel(text);
  AddWidget(PGtkWidget(Result));
end;

function TGTKSimpleWindow.AddEntry(const placeholder: string): PGtkEntry;
begin
  Result := TPasGTK4.CreateEntry;
  if placeholder <> '' then
    TPasGTK4.SetEntryPlaceholder(Result, placeholder);
  AddWidget(PGtkWidget(Result));
end;

{ TGTKGridWindow }

constructor TGTKGridWindow.Create(const app_id: string);
begin
  inherited Create(app_id);
  FMainGrid := nil;
end;

procedure TGTKGridWindow.SetupWindow;
begin
  // Create grid
  FMainGrid := TPasGTK4.CreateGrid;
  
  // Set margins
  TPasGTK4.SetWidgetMargins(PGtkWidget(FMainGrid), 10, 10, 10, 10);
  
  // Add to window
  TPasGTK4.SetWindowChild(FWindow, PGtkWidget(FMainGrid));
end;

procedure TGTKGridWindow.AttachWidget(widget: PGtkWidget; left, top, w, h: Integer);
begin
  if FMainGrid = nil then
  begin
    WriteLn('Window not initialized');
    Exit;
  end;
  TPasGTK4.AttachToGrid(FMainGrid, widget, left, top, w, h);
end;

function TGTKGridWindow.AttachButton(const text: string; left, top, w, h: Integer; callback: TGtkCallback; data: Pointer): PGtkButton;
begin
  Result := TPasGTK4.CreateButton(text);
  if Assigned(callback) then
    TPasGTK4.ConnectSignal(PGtkWidget(Result), 'clicked', callback, data);
  AttachWidget(PGtkWidget(Result), left, top, w, h);
end;

function TGTKGridWindow.AttachLabel(const text: string; left, top, w, h: Integer): PGtkLabel;
begin
  Result := TPasGTK4.CreateLabel(text);
  AttachWidget(PGtkWidget(Result), left, top, w, h);
end;

function TGTKGridWindow.AttachEntry(left, top, w, h: Integer; const placeholder: string): PGtkEntry;
begin
  Result := TPasGTK4.CreateEntry;
  if placeholder <> '' then
    TPasGTK4.SetEntryPlaceholder(Result, placeholder);
  AttachWidget(PGtkWidget(Result), left, top, w, h);
end;

{ TGTKMenuWindow }

constructor TGTKMenuWindow.Create(const app_id: string; vertical: Boolean; widget_spacing: Integer);
begin
  inherited Create(app_id, vertical, widget_spacing);
  FMenuBar := nil;
  SetLength(FMenus, 0);
  FMenuCount := 0;
  FWarningShown := False;
end;

procedure TGTKMenuWindow.SetupWindow;
begin
  // Call inherited setup first
  inherited SetupWindow;
  
  // GTK4 doesn't have traditional menu bars, show warning once
  if not FWarningShown then
  begin
    WriteLn('INFO: GTK4 Menu compatibility mode - traditional menus are deprecated in GTK4');
    WriteLn('INFO: Consider using modern HeaderBar with menu buttons instead');
    FWarningShown := True;
  end;
  
  // Create a simple label as menu placeholder
  FMenuBar := PGtkWidget(TPasGTK4.CreateLabel('[Menu Bar - GTK4 Compatible Mode]'));
  if FMenuBar <> nil then
  begin
    TPasGTK4.PrependToBox(MainBox, FMenuBar);
  end;
end;

function TGTKMenuWindow.CreateMenu(const menu_title: string): PGMenuModel;
begin
  WriteLn('INFO: Creating menu "', menu_title, '" (GTK4 compatibility mode)');
  
  // Create GMenu (modern GTK4 approach)
  Result := TPasGTK4.CreateGMenu;
  if Result = nil then
  begin
    WriteLn('WARNING: Failed to create GMenu, using placeholder');
    Exit;
  end;
  
  // Store menu reference
  Inc(FMenuCount);
  SetLength(FMenus, FMenuCount);
  FMenus[FMenuCount - 1] := Result;
end;

function TGTKMenuWindow.AddMenuItem(menu: PGMenuModel; const text: string; callback: TGtkCallback; data: Pointer): PGtkWidget;
var
  action_name: string;
  button: PGtkButton;
begin
  Result := nil;
  
  if menu = nil then
  begin
    WriteLn('INFO: Menu is nil, creating standalone button for "', text, '"');
    // Create a button as fallback
    button := TPasGTK4.CreateButton(text);
    if Assigned(callback) and (button <> nil) then
      TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', callback, data);
    Result := PGtkWidget(button);
    Exit;
  end;
  
  // For GMenu, create action-based menu item
  action_name := 'menu.' + StringReplace(LowerCase(text), ' ', '_', [rfReplaceAll]);
  action_name := StringReplace(action_name, '&', '', [rfReplaceAll]);
  
  WriteLn('INFO: Adding menu item "', text, '" with action "', action_name, '"');
  TPasGTK4.AppendToGMenu(menu, text, action_name);
  
  // Create a button for demonstration
  button := TPasGTK4.CreateButton(text);
  if Assigned(callback) and (button <> nil) then
    TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', callback, data);
  Result := PGtkWidget(button);
end;

function TGTKMenuWindow.AddMenuItemWithMnemonic(menu: PGMenuModel; const text: string; callback: TGtkCallback; data: Pointer): PGtkWidget;
begin
  // Just call regular AddMenuItem for GTK4 compatibility
  Result := AddMenuItem(menu, text, callback, data);
end;

function TGTKMenuWindow.AddCheckMenuItem(menu: PGMenuModel; const text: string; callback: TGtkCallback; data: Pointer): PGtkWidget;
var
  button: PGtkButton;
begin
  WriteLn('INFO: Creating check menu item "', text, '" as button (GTK4 compatibility)');
  
  // Create toggle button instead of check menu item
  button := TPasGTK4.CreateButton('☐ ' + text);  // Use checkbox symbol
  if Assigned(callback) and (button <> nil) then
    TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', callback, data);
  
  Result := PGtkWidget(button);
end;

function TGTKMenuWindow.AddRadioMenuItem(menu: PGMenuModel; const text: string; group: Pointer; callback: TGtkCallback; data: Pointer): PGtkWidget;
var
  button: PGtkButton;
begin
  WriteLn('INFO: Creating radio menu item "', text, '" as button (GTK4 compatibility)');
  
  // Create button with radio symbol
  button := TPasGTK4.CreateButton('○ ' + text);  // Use radio button symbol
  if Assigned(callback) and (button <> nil) then
    TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', callback, data);
  
  Result := PGtkWidget(button);
end;

procedure TGTKMenuWindow.AddSeparatorMenuItem(menu: PGMenuModel);
begin
  WriteLn('INFO: Separator menu item requested (GTK4 compatibility - no action taken)');
  // GTK4 GMenu handles separators automatically in some cases
end;

function TGTKMenuWindow.CreateSubmenu(parent_menu: PGMenuModel; const submenu_title: string): PGMenuModel;
begin
  WriteLn('INFO: Creating submenu "', submenu_title, '" (GTK4 compatibility)');
  
  // Create new GMenu for submenu
  Result := TPasGTK4.CreateGMenu;
  if Result = nil then
  begin
    WriteLn('WARNING: Failed to create submenu GMenu');
    Exit;
  end;
  
  // Add submenu to parent menu
  if parent_menu <> nil then
    TPasGTK4.AppendSubmenuToGMenu(parent_menu, submenu_title, Result);
  
  // Store submenu reference
  Inc(FMenuCount);
  SetLength(FMenus, FMenuCount);
  FMenus[FMenuCount - 1] := Result;
end;

{ TGTKModernWindow }

constructor TGTKModernWindow.Create(const app_id: string);
begin
  inherited Create(app_id);
  FHeaderBar := nil;
  FMainBox := nil;
  FActionGroup := nil;
  SetLength(FMenus, 0);
  FMenuCount := 0;
end;

procedure TGTKModernWindow.SetupWindow;
var
  titleLabel: PGtkLabel;
begin
  // Create HeaderBar
  FHeaderBar := TPasGTK4.CreateHeaderBar;
  if FHeaderBar = nil then
  begin
    WriteLn('ERROR: Failed to create HeaderBar');
    Exit;
  end;
  
  // Set title
  titleLabel := TPasGTK4.CreateLabel(Title);
  TPasGTK4.SetHeaderBarTitle(FHeaderBar, PGtkWidget(titleLabel));
  
  // Set HeaderBar as titlebar
  TPasGTK4.SetWindowTitlebar(GetWindow, PGtkWidget(FHeaderBar));
  
  // Create action group
  FActionGroup := TPasGTK4.CreateSimpleActionGroup;
  if FActionGroup <> nil then
    TPasGTK4.InsertActionGroup(PGtkWidget(GetWindow), 'win', PGActionGroup(FActionGroup));
  
  // Create main content area
  FMainBox := TPasGTK4.CreateVerticalBox(10);
  TPasGTK4.SetWidgetMargins(PGtkWidget(FMainBox), 20, 20, 20, 20);
  
  // Set main content
  TPasGTK4.SetWindowChild(GetWindow, PGtkWidget(FMainBox));
  
  WriteLn('Modern GTK4 window with HeaderBar created successfully');
end;

procedure TGTKModernWindow.AddHeaderBarButton(button: PGtkWidget; pack_start: Boolean);
begin
  if (FHeaderBar = nil) or (button = nil) then
  begin
    WriteLn('ERROR: HeaderBar or button is nil');
    Exit;
  end;
  
  if pack_start then
    TPasGTK4.HeaderBarPackStart(FHeaderBar, button)
  else
    TPasGTK4.HeaderBarPackEnd(FHeaderBar, button);
end;

function TGTKModernWindow.CreateHeaderBarMenu(const menu_title: string): PGMenuModel;
var
  menuButton: PGtkMenuButton;
  menu: PGMenuModel;
begin
  Result := nil;
  
  // Create menu model
  menu := TPasGTK4.CreateGMenu;
  if menu = nil then
  begin
    WriteLn('ERROR: Failed to create GMenu for HeaderBar');
    Exit;
  end;
  
  // Create menu button
  menuButton := TPasGTK4.CreateMenuButton;
  if menuButton = nil then
  begin
    WriteLn('ERROR: Failed to create MenuButton for HeaderBar');
    Exit;
  end;
  
  // Set button label
  TPasGTK4.ConnectSignal(PGtkWidget(menuButton), 'notify::active', nil, nil);
  
  // Set menu model to button
  TPasGTK4.SetMenuButtonModel(menuButton, menu);
  
  // Add button to HeaderBar
  AddHeaderBarButton(PGtkWidget(menuButton), True);
  
  // Store menu info
  Inc(FMenuCount);
  SetLength(FMenus, FMenuCount);
  FMenus[FMenuCount - 1].Button := menuButton;
  FMenus[FMenuCount - 1].Model := menu;
  FMenus[FMenuCount - 1].MenuTitle := menu_title;
  
  Result := menu;
  WriteLn('HeaderBar menu "', menu_title, '" created successfully');
end;

function TGTKModernWindow.AddMenuAction(menu: PGMenuModel; const label_text, action_name: string; callback: TGActionCallback; data: Pointer): Boolean;
var
  action: PGSimpleAction;
  full_action_name: string;
begin
  Result := False;
  
  if menu = nil then
  begin
    WriteLn('ERROR: Menu is nil');
    Exit;
  end;
  
  // Create action
  action := TPasGTK4.CreateSimpleAction(action_name);
  if action = nil then
  begin
    WriteLn('ERROR: Failed to create action "', action_name, '"');
    Exit;
  end;
  
  // Connect callback if provided
  if Assigned(callback) then
    TPasGTK4.ConnectActionSignal(Pointer(action), 'activate', callback, data);
  
  // Add action to action group
  if FActionGroup <> nil then
    TPasGTK4.AddActionToMap(PGActionGroup(FActionGroup), action);
  
  // Add menu item
  full_action_name := 'win.' + action_name;
  TPasGTK4.AppendToGMenu(menu, label_text, full_action_name);
  
  Result := True;
  WriteLn('Menu action "', label_text, '" -> "', full_action_name, '" added successfully');
end;

procedure TGTKModernWindow.AddWidget(widget: PGtkWidget);
begin
  if FMainBox = nil then
  begin
    WriteLn('ERROR: MainBox not created');
    Exit;
  end;
  TPasGTK4.AddToBox(FMainBox, widget);
end;

function TGTKModernWindow.AddButton(const text: string; callback: TGtkCallback; data: Pointer): PGtkButton;
begin
  Result := TPasGTK4.CreateButton(text);
  if Assigned(callback) then
    TPasGTK4.ConnectSignal(PGtkWidget(Result), 'clicked', callback, data);
  AddWidget(PGtkWidget(Result));
end;

function TGTKModernWindow.AddLabel(const text: string): PGtkLabel;
begin
  Result := TPasGTK4.CreateLabel(text);
  AddWidget(PGtkWidget(Result));
end;

function TGTKModernWindow.AddEntry(const placeholder: string): PGtkEntry;
begin
  Result := TPasGTK4.CreateEntry;
  if placeholder <> '' then
    TPasGTK4.SetEntryPlaceholder(Result, placeholder);
  AddWidget(PGtkWidget(Result));
end;

end.