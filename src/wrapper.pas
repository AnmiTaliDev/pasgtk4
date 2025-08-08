unit wrapper;

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

// Conditional compilation for debug output
{$IFDEF DEBUG}
  {$DEFINE ENABLE_DEBUG_OUTPUT}
{$ENDIF}

interface

uses
  ctypes, dynlibs, SysUtils, Math;

const
  PASGTK4_VERSION = '1.0.0';

{$IFDEF ENABLE_DEBUG_OUTPUT}
  // Debug output enabled
  procedure DebugWriteLn(const msg: string);
{$ELSE}
  // Empty stub for release build
  procedure DebugWriteLn(const msg: string);
{$ENDIF}

type
  // GTK4 basic types
  gboolean = LongBool;
  PGtkWidget = Pointer;
  PGtkApplication = Pointer;
  PGtkWindow = Pointer;
  PGtkButton = Pointer;
  PGtkBox = Pointer;
  PGtkLabel = Pointer;
  PGtkEntry = Pointer;
  PGtkGrid = Pointer;
  PGtkMenuButton = Pointer;
  PGtkPopover = Pointer;
  PGtkPopoverMenu = Pointer;
  PGtkHeaderBar = Pointer;
  PGActionGroup = Pointer;
  PGSimpleActionGroup = Pointer;
  PGtkMenuBar = Pointer;
  PGtkMenu = Pointer;
  PGtkMenuItem = Pointer;
  PGtkImageMenuItem = Pointer;
  PGtkSeparatorMenuItem = Pointer;
  PGtkCheckMenuItem = Pointer;
  PGtkRadioMenuItem = Pointer;
  PGMenuModel = Pointer;
  PGMenuItem = Pointer;
  PGSimpleAction = Pointer;
  
  // LibAdwaita types
  PAdwApplication = Pointer;
  PAdwApplicationWindow = Pointer;
  PAdwHeaderBar = Pointer;
  PAdwToastOverlay = Pointer;
  PAdwToast = Pointer;
  
  // Enumerations
  GtkOrientation = (
    GTK_ORIENTATION_HORIZONTAL = 0,
    GTK_ORIENTATION_VERTICAL = 1
  );
  
  GtkAlign = (
    GTK_ALIGN_FILL = 0,
    GTK_ALIGN_START = 1,
    GTK_ALIGN_END = 2,
    GTK_ALIGN_CENTER = 3,
    GTK_ALIGN_BASELINE = 4
  );
  
  GtkJustification = (
    GTK_JUSTIFY_LEFT = 0,
    GTK_JUSTIFY_RIGHT = 1,
    GTK_JUSTIFY_CENTER = 2,
    GTK_JUSTIFY_FILL = 3
  );
  
  // Callback types
  TGtkCallback = procedure(widget: PGtkWidget; data: Pointer); cdecl;
  TGtkApplicationCallback = procedure(app: PGtkApplication; data: Pointer); cdecl;
  TGActionCallback = procedure(action: Pointer; parameter: Pointer; data: Pointer); cdecl;
  
  // Class for working with GTK4
  TPasGTK4 = class
  private
    class var FLibHandle: TLibHandle;
    class var FAdwLibHandle: TLibHandle;
    class var FInitialized: Boolean;
    class var FAdwInitialized: Boolean;
    
    // GTK4 function pointers
    class var gtk_init_check: function: gboolean; cdecl;
    class var gtk_application_new: function(application_id: PChar; flags: cuint): PGtkApplication; cdecl;
    class var gtk_application_window_new: function(application: PGtkApplication): PGtkWindow; cdecl;
    class var gtk_window_set_title: procedure(window: PGtkWindow; title: PChar); cdecl;
    class var gtk_window_set_default_size: procedure(window: PGtkWindow; width, height: cint); cdecl;
    class var gtk_window_present: procedure(window: PGtkWindow); cdecl;
    class var gtk_window_set_child: procedure(window: PGtkWindow; child: PGtkWidget); cdecl;
    class var gtk_button_new_with_label: function(label_text: PChar): PGtkButton; cdecl;
    class var gtk_box_new: function(orientation: GtkOrientation; spacing: cint): PGtkBox; cdecl;
    class var gtk_box_append: procedure(box: PGtkBox; child: PGtkWidget); cdecl;
    class var gtk_box_prepend: procedure(box: PGtkBox; child: PGtkWidget); cdecl;
    class var gtk_label_new: function(str: PChar): PGtkLabel; cdecl;
    class var gtk_label_set_text: procedure(label_widget: PGtkLabel; str: PChar); cdecl;
    class var gtk_label_set_justify: procedure(label_widget: PGtkLabel; jtype: GtkJustification); cdecl;
    class var gtk_entry_new: function: PGtkEntry; cdecl;
    class var gtk_entry_set_text: procedure(entry: PGtkEntry; text: PChar); cdecl;
    class var gtk_editable_set_text: procedure(editable: PGtkEntry; text: PChar); cdecl;
    class var gtk_entry_get_text: function(entry: PGtkEntry): PChar; cdecl;
    class var gtk_editable_get_text: function(editable: PGtkEntry): PChar; cdecl;
    class var gtk_entry_set_placeholder_text: procedure(entry: PGtkEntry; text: PChar); cdecl;
    class var gtk_grid_new: function: PGtkGrid; cdecl;
    class var gtk_grid_attach: procedure(grid: PGtkGrid; child: PGtkWidget; left, top, width, height: cint); cdecl;
    class var gtk_widget_set_halign: procedure(widget: PGtkWidget; align: GtkAlign); cdecl;
    class var gtk_widget_set_valign: procedure(widget: PGtkWidget; align: GtkAlign); cdecl;
    class var gtk_widget_set_margin_top: procedure(widget: PGtkWidget; margin: cint); cdecl;
    class var gtk_widget_set_margin_bottom: procedure(widget: PGtkWidget; margin: cint); cdecl;
    class var gtk_widget_set_margin_start: procedure(widget: PGtkWidget; margin: cint); cdecl;
    class var gtk_widget_set_margin_end: procedure(widget: PGtkWidget; margin: cint); cdecl;
    class var g_signal_connect_data: function(instance: Pointer; detailed_signal: PChar; 
      c_handler: Pointer; data: Pointer; destroy_data: Pointer; 
      connect_flags: cuint): culong; cdecl;
    class var g_application_run: function(application: Pointer; argc: cint; argv: PPChar): cint; cdecl;
    
    // Modern GTK4 functions
    class var gtk_header_bar_new: function: PGtkHeaderBar; cdecl;
    class var gtk_header_bar_set_title_widget: procedure(bar: PGtkHeaderBar; title_widget: PGtkWidget); cdecl;
    class var gtk_header_bar_pack_start: procedure(bar: PGtkHeaderBar; child: PGtkWidget); cdecl;
    class var gtk_header_bar_pack_end: procedure(bar: PGtkHeaderBar; child: PGtkWidget); cdecl;
    class var gtk_menu_button_new: function: PGtkMenuButton; cdecl;
    class var gtk_menu_button_set_popover: procedure(button: PGtkMenuButton; popover: PGtkPopover); cdecl;
    class var gtk_menu_button_set_menu_model: procedure(button: PGtkMenuButton; menu_model: PGMenuModel); cdecl;
    class var gtk_popover_menu_new_from_model: function(model: PGMenuModel): PGtkPopoverMenu; cdecl;
    class var gtk_window_set_titlebar: procedure(window: PGtkWindow; titlebar: PGtkWidget); cdecl;
    class var g_simple_action_group_new: function: PGSimpleActionGroup; cdecl;
    class var g_action_map_add_action_entries: procedure(action_map: PGActionGroup; entries: Pointer; n_entries: cint; user_data: Pointer); cdecl;
    class var gtk_widget_insert_action_group: procedure(widget: PGtkWidget; name: PChar; group: PGActionGroup); cdecl;
    
    // Menu functions
    class var gtk_menu_bar_new: function: PGtkMenuBar; cdecl;
    class var gtk_menu_new: function: PGtkMenu; cdecl;
    class var gtk_menu_item_new: function: PGtkMenuItem; cdecl;
    class var gtk_menu_item_new_with_label: function(label_text: PChar): PGtkMenuItem; cdecl;
    class var gtk_menu_item_new_with_mnemonic: function(label_text: PChar): PGtkMenuItem; cdecl;
    class var gtk_image_menu_item_new: function: PGtkImageMenuItem; cdecl;
    class var gtk_image_menu_item_new_with_label: function(label_text: PChar): PGtkImageMenuItem; cdecl;
    class var gtk_separator_menu_item_new: function: PGtkSeparatorMenuItem; cdecl;
    class var gtk_check_menu_item_new: function: PGtkCheckMenuItem; cdecl;
    class var gtk_check_menu_item_new_with_label: function(label_text: PChar): PGtkCheckMenuItem; cdecl;
    class var gtk_radio_menu_item_new: function(group: Pointer): PGtkRadioMenuItem; cdecl;
    class var gtk_radio_menu_item_new_with_label: function(group: Pointer; label_text: PChar): PGtkRadioMenuItem; cdecl;
    class var gtk_menu_item_set_submenu: procedure(menu_item: PGtkMenuItem; submenu: PGtkWidget); cdecl;
    class var gtk_menu_shell_append: procedure(menu_shell: Pointer; child: PGtkWidget); cdecl;
    class var gtk_menu_shell_prepend: procedure(menu_shell: Pointer; child: PGtkWidget); cdecl;
    class var gtk_check_menu_item_set_active: procedure(check_menu_item: PGtkCheckMenuItem; is_active: gboolean); cdecl;
    class var gtk_check_menu_item_get_active: function(check_menu_item: PGtkCheckMenuItem): gboolean; cdecl;
    class var g_menu_new: function: PGMenuModel; cdecl;
    class var g_menu_append: procedure(menu: PGMenuModel; label_text: PChar; detailed_action: PChar); cdecl;
    class var g_menu_append_submenu: procedure(menu: PGMenuModel; label_text: PChar; submenu: PGMenuModel); cdecl;
    class var g_simple_action_new: function(name: PChar; parameter_type: Pointer): PGSimpleAction; cdecl;
    class var g_action_map_add_action: procedure(action_map: Pointer; action: Pointer); cdecl;
    
    // LibAdwaita functions
    class var adw_init: procedure; cdecl;
    class var adw_application_new: function(application_id: PChar; flags: cuint): PAdwApplication; cdecl;
    class var adw_application_window_new: function(app: PAdwApplication): PAdwApplicationWindow; cdecl;
    class var adw_application_window_set_content: procedure(window: PAdwApplicationWindow; content: PGtkWidget); cdecl;
    class var adw_header_bar_new: function: PAdwHeaderBar; cdecl;
    class var adw_header_bar_set_title_widget: procedure(header_bar: PAdwHeaderBar; title_widget: PGtkWidget); cdecl;
    class var adw_toast_overlay_new: function: PAdwToastOverlay; cdecl;
    class var adw_toast_overlay_set_child: procedure(overlay: PAdwToastOverlay; child: PGtkWidget); cdecl;
    class var adw_toast_new: function(title: PChar): PAdwToast; cdecl;
    class var adw_toast_overlay_add_toast: procedure(overlay: PAdwToastOverlay; toast: PAdwToast); cdecl;
    
    class function LoadLibrary: Boolean;
    class function LoadAdwaitaLibrary: Boolean;
    class function GetProcAddr(const name: string): Pointer;
    class function GetAdwProcAddr(const name: string): Pointer;
    
  public
    // Initialization and finalization
    class function Initialize: Boolean;
    class function InitializeAdwaita: Boolean;
    class procedure Finalize;
    class function IsInitialized: Boolean;
    class function IsAdwaitaInitialized: Boolean;
    class function GetVersion: string;
    
    // Application
    class function CreateApplication(const app_id: string): PGtkApplication;
    class function RunApplication(app: PGtkApplication): Integer;
    class procedure ConnectApplicationSignal(app: PGtkApplication; const signal: string; callback: TGtkApplicationCallback; data: Pointer = nil);
    
    // Windows
    class function CreateWindow(app: PGtkApplication): PGtkWindow;
    class procedure SetWindowTitle(window: PGtkWindow; const title: string);
    class procedure SetWindowSize(window: PGtkWindow; width, height: Integer);
    class procedure SetWindowChild(window: PGtkWindow; child: PGtkWidget);
    class procedure ShowWindow(window: PGtkWindow);
    
    // Widgets
    class function CreateButton(const text: string): PGtkButton;
    class function CreateLabel(const text: string): PGtkLabel;
    class procedure SetLabelText(label_widget: PGtkLabel; const text: string);
    class procedure SetLabelJustify(label_widget: PGtkLabel; justify: GtkJustification);
    class function CreateEntry: PGtkEntry;
    class procedure SetEntryText(entry: PGtkEntry; const text: string);
    class function GetEntryText(entry: PGtkEntry): string;
    class procedure SetEntryPlaceholder(entry: PGtkEntry; const text: string);
    
    // Containers
    class function CreateBox(orientation: GtkOrientation; spacing: Integer): PGtkBox;
    class function CreateVerticalBox(spacing: Integer): PGtkBox;
    class function CreateHorizontalBox(spacing: Integer): PGtkBox;
    class procedure AddToBox(box: PGtkBox; widget: PGtkWidget);
    class procedure PrependToBox(box: PGtkBox; widget: PGtkWidget);
    class function CreateGrid: PGtkGrid;
    class procedure AttachToGrid(grid: PGtkGrid; widget: PGtkWidget; left, top, width, height: Integer);
    
    // Alignment and margins
    class procedure SetWidgetAlign(widget: PGtkWidget; halign, valign: GtkAlign);
    class procedure SetWidgetMargins(widget: PGtkWidget; top, bottom, start, end_margin: Integer);
    
    // Signals
    class procedure ConnectSignal(widget: PGtkWidget; const signal: string; callback: TGtkCallback; data: Pointer = nil);
    class procedure ConnectActionSignal(action: Pointer; const signal: string; callback: TGActionCallback; data: Pointer = nil);
    
    // Modern GTK4 methods
    class function CreateHeaderBar: PGtkHeaderBar;
    class procedure SetHeaderBarTitle(bar: PGtkHeaderBar; title_widget: PGtkWidget);
    class procedure HeaderBarPackStart(bar: PGtkHeaderBar; child: PGtkWidget);
    class procedure HeaderBarPackEnd(bar: PGtkHeaderBar; child: PGtkWidget);
    class function CreateMenuButton: PGtkMenuButton;
    class procedure SetMenuButtonModel(button: PGtkMenuButton; model: PGMenuModel);
    class function CreatePopoverMenu(model: PGMenuModel): PGtkPopoverMenu;
    class procedure SetWindowTitlebar(window: PGtkWindow; titlebar: PGtkWidget);
    class function CreateSimpleActionGroup: PGSimpleActionGroup;
    class procedure InsertActionGroup(widget: PGtkWidget; const name: string; group: PGActionGroup);
    
    // Menus
    class function CreateMenuBar: PGtkMenuBar;
    class function CreateMenu: PGtkMenu;
    class function CreateMenuItem(const text: string): PGtkMenuItem;
    class function CreateMenuItemWithMnemonic(const text: string): PGtkMenuItem;
    class function CreateImageMenuItem(const text: string): PGtkImageMenuItem;
    class function CreateSeparatorMenuItem: PGtkSeparatorMenuItem;
    class function CreateCheckMenuItem(const text: string): PGtkCheckMenuItem;
    class function CreateRadioMenuItem(group: Pointer; const text: string): PGtkRadioMenuItem;
    class procedure SetMenuItemSubmenu(menu_item: PGtkMenuItem; submenu: PGtkWidget);
    class procedure AppendToMenuShell(menu_shell: Pointer; item: PGtkWidget);
    class procedure PrependToMenuShell(menu_shell: Pointer; item: PGtkWidget);
    class procedure SetCheckMenuItemActive(check_item: PGtkCheckMenuItem; active: Boolean);
    class function GetCheckMenuItemActive(check_item: PGtkCheckMenuItem): Boolean;
    class function CreateGMenu: PGMenuModel;
    class procedure AppendToGMenu(menu: PGMenuModel; const label_text, action: string);
    class procedure AppendSubmenuToGMenu(menu: PGMenuModel; const label_text: string; submenu: PGMenuModel);
    class function CreateSimpleAction(const name: string): PGSimpleAction;
    class procedure AddActionToMap(action_map: Pointer; action: PGSimpleAction);
    
    // LibAdwaita methods
    class function CreateAdwApplication(const app_id: string): PAdwApplication;
    class function CreateAdwWindow(app: PAdwApplication): PAdwApplicationWindow;
    class procedure SetAdwWindowContent(window: PAdwApplicationWindow; content: PGtkWidget);
    class function CreateAdwHeaderBar: PAdwHeaderBar;
    class procedure SetAdwHeaderBarTitle(header_bar: PAdwHeaderBar; title_widget: PGtkWidget);
    class function CreateToastOverlay: PAdwToastOverlay;
    class procedure SetToastOverlayChild(overlay: PAdwToastOverlay; child: PGtkWidget);
    class function CreateToast(const title: string): PAdwToast;
    class procedure ShowToast(overlay: PAdwToastOverlay; toast: PAdwToast);
  end;

implementation

{$IFDEF ENABLE_DEBUG_OUTPUT}
procedure DebugWriteLn(const msg: string);
begin
  WriteLn('[DEBUG] ', msg);
end;
{$ELSE}
procedure DebugWriteLn(const msg: string);
begin
  // Do nothing in release build
end;
{$ENDIF}

class function TPasGTK4.LoadLibrary: Boolean;
const
  GTK_LIBS: array[0..4] of string = (
    'libgtk-4.so.1',      // Linux
    'libgtk-4.so',        // Linux alternative
    'libgtk-4.dylib',     // macOS
    'gtk-4.dll',          // Windows
    'libgtk-4-1.dll'      // Windows alternative
  );
var
  i: Integer;
begin
  Result := False;
  
  for i := 0 to High(GTK_LIBS) do
  begin
    FLibHandle := dynlibs.LoadLibrary(GTK_LIBS[i]);
    if FLibHandle <> 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TPasGTK4.GetProcAddr(const name: string): Pointer;
begin
  Result := dynlibs.GetProcAddress(FLibHandle, PChar(name));
end;

class function TPasGTK4.LoadAdwaitaLibrary: Boolean;
const
  ADW_LIBS: array[0..3] of string = (
    'libadwaita-1.so.0',    // Linux
    'libadwaita-1.so',      // Linux alternative
    'libadwaita-1.dylib',   // macOS
    'adwaita-1.dll'         // Windows
  );
var
  i: Integer;
begin
  Result := False;
  
  for i := 0 to High(ADW_LIBS) do
  begin
    FAdwLibHandle := dynlibs.LoadLibrary(ADW_LIBS[i]);
    if FAdwLibHandle <> 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TPasGTK4.GetAdwProcAddr(const name: string): Pointer;
begin
  Result := dynlibs.GetProcAddress(FAdwLibHandle, PChar(name));
end;

class function TPasGTK4.Initialize: Boolean;
begin
  Result := False;
  
  if FInitialized then
  begin
    Result := True;
    Exit;
  end;
  
  DebugWriteLn('Loading GTK4 library...');
  if not LoadLibrary then
  begin
    WriteLn('ERROR: Failed to load GTK4 library');
    Exit;
  end;
  
  DebugWriteLn('Loading GTK4 functions...');
  // Load function pointers
  Pointer(gtk_init_check) := GetProcAddr('gtk_init_check');
  Pointer(gtk_application_new) := GetProcAddr('gtk_application_new');
  Pointer(gtk_application_window_new) := GetProcAddr('gtk_application_window_new');
  Pointer(gtk_window_set_title) := GetProcAddr('gtk_window_set_title');
  Pointer(gtk_window_set_default_size) := GetProcAddr('gtk_window_set_default_size');
  Pointer(gtk_window_present) := GetProcAddr('gtk_window_present');
  Pointer(gtk_window_set_child) := GetProcAddr('gtk_window_set_child');
  Pointer(gtk_button_new_with_label) := GetProcAddr('gtk_button_new_with_label');
  Pointer(gtk_box_new) := GetProcAddr('gtk_box_new');
  Pointer(gtk_box_append) := GetProcAddr('gtk_box_append');
  Pointer(gtk_box_prepend) := GetProcAddr('gtk_box_prepend');
  Pointer(gtk_label_new) := GetProcAddr('gtk_label_new');
  Pointer(gtk_label_set_text) := GetProcAddr('gtk_label_set_text');
  Pointer(gtk_label_set_justify) := GetProcAddr('gtk_label_set_justify');
  Pointer(gtk_entry_new) := GetProcAddr('gtk_entry_new');
  Pointer(gtk_entry_set_text) := GetProcAddr('gtk_entry_set_text');
  Pointer(gtk_editable_set_text) := GetProcAddr('gtk_editable_set_text');
  Pointer(gtk_entry_get_text) := GetProcAddr('gtk_entry_get_text');
  Pointer(gtk_editable_get_text) := GetProcAddr('gtk_editable_get_text');
  Pointer(gtk_entry_set_placeholder_text) := GetProcAddr('gtk_entry_set_placeholder_text');
  Pointer(gtk_grid_new) := GetProcAddr('gtk_grid_new');
  Pointer(gtk_grid_attach) := GetProcAddr('gtk_grid_attach');
  Pointer(gtk_widget_set_halign) := GetProcAddr('gtk_widget_set_halign');
  Pointer(gtk_widget_set_valign) := GetProcAddr('gtk_widget_set_valign');
  Pointer(gtk_widget_set_margin_top) := GetProcAddr('gtk_widget_set_margin_top');
  Pointer(gtk_widget_set_margin_bottom) := GetProcAddr('gtk_widget_set_margin_bottom');
  Pointer(gtk_widget_set_margin_start) := GetProcAddr('gtk_widget_set_margin_start');
  Pointer(gtk_widget_set_margin_end) := GetProcAddr('gtk_widget_set_margin_end');
  Pointer(g_signal_connect_data) := GetProcAddr('g_signal_connect_data');
  Pointer(g_application_run) := GetProcAddr('g_application_run');
  
  // Load modern GTK4 function pointers
  Pointer(gtk_header_bar_new) := GetProcAddr('gtk_header_bar_new');
  Pointer(gtk_header_bar_set_title_widget) := GetProcAddr('gtk_header_bar_set_title_widget');
  Pointer(gtk_header_bar_pack_start) := GetProcAddr('gtk_header_bar_pack_start');
  Pointer(gtk_header_bar_pack_end) := GetProcAddr('gtk_header_bar_pack_end');
  Pointer(gtk_menu_button_new) := GetProcAddr('gtk_menu_button_new');
  Pointer(gtk_menu_button_set_popover) := GetProcAddr('gtk_menu_button_set_popover');
  Pointer(gtk_menu_button_set_menu_model) := GetProcAddr('gtk_menu_button_set_menu_model');
  Pointer(gtk_popover_menu_new_from_model) := GetProcAddr('gtk_popover_menu_new_from_model');
  Pointer(gtk_window_set_titlebar) := GetProcAddr('gtk_window_set_titlebar');
  Pointer(g_simple_action_group_new) := GetProcAddr('g_simple_action_group_new');
  Pointer(g_action_map_add_action_entries) := GetProcAddr('g_action_map_add_action_entries');
  Pointer(gtk_widget_insert_action_group) := GetProcAddr('gtk_widget_insert_action_group');
  
  // Load menu function pointers
  Pointer(gtk_menu_bar_new) := GetProcAddr('gtk_menu_bar_new');
  Pointer(gtk_menu_new) := GetProcAddr('gtk_menu_new');
  Pointer(gtk_menu_item_new) := GetProcAddr('gtk_menu_item_new');
  Pointer(gtk_menu_item_new_with_label) := GetProcAddr('gtk_menu_item_new_with_label');
  Pointer(gtk_menu_item_new_with_mnemonic) := GetProcAddr('gtk_menu_item_new_with_mnemonic');
  Pointer(gtk_image_menu_item_new) := GetProcAddr('gtk_image_menu_item_new');
  Pointer(gtk_image_menu_item_new_with_label) := GetProcAddr('gtk_image_menu_item_new_with_label');
  Pointer(gtk_separator_menu_item_new) := GetProcAddr('gtk_separator_menu_item_new');
  Pointer(gtk_check_menu_item_new) := GetProcAddr('gtk_check_menu_item_new');
  Pointer(gtk_check_menu_item_new_with_label) := GetProcAddr('gtk_check_menu_item_new_with_label');
  Pointer(gtk_radio_menu_item_new) := GetProcAddr('gtk_radio_menu_item_new');
  Pointer(gtk_radio_menu_item_new_with_label) := GetProcAddr('gtk_radio_menu_item_new_with_label');
  Pointer(gtk_menu_item_set_submenu) := GetProcAddr('gtk_menu_item_set_submenu');
  Pointer(gtk_menu_shell_append) := GetProcAddr('gtk_menu_shell_append');
  Pointer(gtk_menu_shell_prepend) := GetProcAddr('gtk_menu_shell_prepend');
  Pointer(gtk_check_menu_item_set_active) := GetProcAddr('gtk_check_menu_item_set_active');
  Pointer(gtk_check_menu_item_get_active) := GetProcAddr('gtk_check_menu_item_get_active');
  Pointer(g_menu_new) := GetProcAddr('g_menu_new');
  Pointer(g_menu_append) := GetProcAddr('g_menu_append');
  Pointer(g_menu_append_submenu) := GetProcAddr('g_menu_append_submenu');
  Pointer(g_simple_action_new) := GetProcAddr('g_simple_action_new');
  Pointer(g_action_map_add_action) := GetProcAddr('g_action_map_add_action');
  
  // Check essential functions
  if not (Assigned(gtk_application_new) and 
          Assigned(gtk_application_window_new) and Assigned(gtk_window_set_title) and
          Assigned(g_signal_connect_data) and Assigned(g_application_run)) then
  begin
    WriteLn('ERROR: Failed to load all required GTK4 functions');
    Exit;
  end;
  
  // Initialize GTK4 if function is available
  if Assigned(gtk_init_check) then
  begin
    DebugWriteLn('Initializing GTK4...');
    if not gtk_init_check() then
    begin
      WriteLn('ERROR: Failed to initialize GTK4');
      Exit;
    end;
    DebugWriteLn('GTK4 initialized successfully');
  end else
    DebugWriteLn('GTK4 ready to use');
  
  FInitialized := True;
  Result := True;
end;

class procedure TPasGTK4.Finalize;
begin
  if FAdwLibHandle <> 0 then
  begin
    dynlibs.FreeLibrary(FAdwLibHandle);
    FAdwLibHandle := 0;
  end;
  FAdwInitialized := False;
  
  if FLibHandle <> 0 then
  begin
    dynlibs.FreeLibrary(FLibHandle);
    FLibHandle := 0;
  end;
  FInitialized := False;
end;

class function TPasGTK4.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

class function TPasGTK4.InitializeAdwaita: Boolean;
begin
  Result := False;
  
  if FAdwInitialized then
  begin
    Result := True;
    Exit;
  end;
  
  if not FInitialized then
  begin
    WriteLn('ERROR: GTK4 must be initialized before LibAdwaita');
    Exit;
  end;
  
  DebugWriteLn('Loading LibAdwaita library...');
  if not LoadAdwaitaLibrary then
  begin
    WriteLn('ERROR: Failed to load LibAdwaita library');
    Exit;
  end;
  
  DebugWriteLn('Loading LibAdwaita functions...');
  // Load LibAdwaita function pointers
  Pointer(adw_init) := GetAdwProcAddr('adw_init');
  Pointer(adw_application_new) := GetAdwProcAddr('adw_application_new');
  Pointer(adw_application_window_new) := GetAdwProcAddr('adw_application_window_new');
  Pointer(adw_application_window_set_content) := GetAdwProcAddr('adw_application_window_set_content');
  Pointer(adw_header_bar_new) := GetAdwProcAddr('adw_header_bar_new');
  Pointer(adw_header_bar_set_title_widget) := GetAdwProcAddr('adw_header_bar_set_title_widget');
  Pointer(adw_toast_overlay_new) := GetAdwProcAddr('adw_toast_overlay_new');
  Pointer(adw_toast_overlay_set_child) := GetAdwProcAddr('adw_toast_overlay_set_child');
  Pointer(adw_toast_new) := GetAdwProcAddr('adw_toast_new');
  Pointer(adw_toast_overlay_add_toast) := GetAdwProcAddr('adw_toast_overlay_add_toast');
  
  // Check essential functions
  if not (Assigned(adw_init) and Assigned(adw_application_new) and 
          Assigned(adw_application_window_new) and Assigned(adw_header_bar_new)) then
  begin
    WriteLn('ERROR: Failed to load all required LibAdwaita functions');
    Exit;
  end;
  
  // Initialize LibAdwaita
  DebugWriteLn('Initializing LibAdwaita...');
  try
    adw_init();
    DebugWriteLn('LibAdwaita initialized successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while initializing LibAdwaita: ', E.Message);
      Exit;
    end;
  end;
  
  FAdwInitialized := True;
  Result := True;
end;

class function TPasGTK4.IsAdwaitaInitialized: Boolean;
begin
  Result := FAdwInitialized;
end;

class function TPasGTK4.GetVersion: string;
begin
  Result := PASGTK4_VERSION;
end;

// Application
class function TPasGTK4.CreateApplication(const app_id: string): PGtkApplication;
begin
  if not Assigned(gtk_application_new) then
  begin
    WriteLn('ERROR: Function gtk_application_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_application_new(PChar(app_id), 0);
    if Result = nil then
      WriteLn('ERROR: gtk_application_new returned nil');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating application: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.RunApplication(app: PGtkApplication): Integer;
var
  oldFPUMask: TFPUExceptionMask;
begin
  if app = nil then
  begin
    WriteLn('ERROR: Application not created');
    Result := -1;
    Exit;
  end;
  
  if not Assigned(g_application_run) then
  begin
    WriteLn('ERROR: Function g_application_run not loaded');
    Result := -1;
    Exit;
  end;
  
  // Mask FPU exceptions that may occur in GTK4
  oldFPUMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  
  try
    Result := g_application_run(app, 0, nil);
  except
    on E: Exception do
    begin
      WriteLn('ERROR while running application: ', E.Message);
      Result := -1;
    end;
  end;
  
  // Restore FPU mask
  SetExceptionMask(oldFPUMask);
end;

class procedure TPasGTK4.ConnectApplicationSignal(app: PGtkApplication; const signal: string; callback: TGtkApplicationCallback; data: Pointer);
begin
  if not Assigned(g_signal_connect_data) or not Assigned(callback) or (app = nil) then
  begin
    WriteLn('ERROR: Invalid parameters for connecting application signal');
    Exit;
  end;
  g_signal_connect_data(app, PChar(signal), Pointer(callback), data, nil, 0);
end;

class procedure TPasGTK4.ConnectActionSignal(action: Pointer; const signal: string; callback: TGActionCallback; data: Pointer);
begin
  if not Assigned(g_signal_connect_data) or not Assigned(callback) or (action = nil) then
  begin
    WriteLn('ERROR: Invalid parameters for connecting action signal');
    Exit;
  end;
  g_signal_connect_data(action, PChar(signal), Pointer(callback), data, nil, 0);
end;

// Windows
class function TPasGTK4.CreateWindow(app: PGtkApplication): PGtkWindow;
begin
  Result := gtk_application_window_new(app);
end;

class procedure TPasGTK4.SetWindowTitle(window: PGtkWindow; const title: string);
begin
  if (window = nil) or not Assigned(gtk_window_set_title) then
  begin
    WriteLn('ERROR: window = nil or function gtk_window_set_title not loaded');
    Exit;
  end;
  gtk_window_set_title(window, PChar(title));
end;

class procedure TPasGTK4.SetWindowSize(window: PGtkWindow; width, height: Integer);
begin
  if (window = nil) or not Assigned(gtk_window_set_default_size) then
  begin
    WriteLn('ERROR: window = nil or function gtk_window_set_default_size not loaded');
    Exit;
  end;
  gtk_window_set_default_size(window, width, height);
end;

class procedure TPasGTK4.SetWindowChild(window: PGtkWindow; child: PGtkWidget);
begin
  if (window = nil) or not Assigned(gtk_window_set_child) then
  begin
    WriteLn('ERROR: window = nil or function gtk_window_set_child not loaded');
    Exit;
  end;
  gtk_window_set_child(window, child);
end;

class procedure TPasGTK4.ShowWindow(window: PGtkWindow);
begin
  if (window = nil) or not Assigned(gtk_window_present) then
  begin
    WriteLn('ERROR: window = nil or function gtk_window_present not loaded');
    Exit;
  end;
  gtk_window_present(window);
end;

// Widgets
class function TPasGTK4.CreateButton(const text: string): PGtkButton;
begin
  Result := gtk_button_new_with_label(PChar(text));
end;

class function TPasGTK4.CreateLabel(const text: string): PGtkLabel;
begin
  Result := gtk_label_new(PChar(text));
end;

class procedure TPasGTK4.SetLabelText(label_widget: PGtkLabel; const text: string);
begin
  gtk_label_set_text(label_widget, PChar(text));
end;

class procedure TPasGTK4.SetLabelJustify(label_widget: PGtkLabel; justify: GtkJustification);
begin
  gtk_label_set_justify(label_widget, justify);
end;

class function TPasGTK4.CreateEntry: PGtkEntry;
begin
  if not Assigned(gtk_entry_new) then
  begin
    WriteLn('ERROR: gtk_entry_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_entry_new();
    if Result = nil then
      WriteLn('ERROR: gtk_entry_new returned nil')
    else
      DebugWriteLn('Entry created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating entry: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetEntryText(entry: PGtkEntry; const text: string);
begin
  if entry = nil then
  begin
    WriteLn('ERROR: entry = nil in SetEntryText');
    Exit;
  end;
  
  try
    // Try gtk_editable_set_text first (GTK4 preferred method)
    if Assigned(gtk_editable_set_text) then
      gtk_editable_set_text(entry, PChar(text))
    // Fallback to gtk_entry_set_text
    else if Assigned(gtk_entry_set_text) then
      gtk_entry_set_text(entry, PChar(text))
    else
      WriteLn('ERROR: neither gtk_editable_set_text nor gtk_entry_set_text loaded');
  except
    on E: Exception do
      WriteLn('ERROR while setting entry text: ', E.Message);
  end;
end;

class function TPasGTK4.GetEntryText(entry: PGtkEntry): string;
var
  text_ptr: PChar;
begin
  Result := '';
  if entry = nil then
  begin
    WriteLn('ERROR: entry = nil');
    Exit;
  end;
  
  try
    // Try gtk_editable_get_text first (GTK4 preferred method)
    if Assigned(gtk_editable_get_text) then
    begin
      text_ptr := gtk_editable_get_text(entry);
      if text_ptr <> nil then
        Result := string(text_ptr)
      else
        Result := '';
    end
    // Fallback to gtk_entry_get_text
    else if Assigned(gtk_entry_get_text) then
    begin
      text_ptr := gtk_entry_get_text(entry);
      if text_ptr <> nil then
        Result := string(text_ptr)
      else
        Result := '';
    end
    else
    begin
      WriteLn('ERROR: neither gtk_editable_get_text nor gtk_entry_get_text loaded');
      Exit;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR while getting entry text: ', E.Message);
      Result := '';
    end;
  end;
end;

class procedure TPasGTK4.SetEntryPlaceholder(entry: PGtkEntry; const text: string);
begin
  gtk_entry_set_placeholder_text(entry, PChar(text));
end;

// Containers
class function TPasGTK4.CreateBox(orientation: GtkOrientation; spacing: Integer): PGtkBox;
begin
  Result := gtk_box_new(orientation, spacing);
end;

class function TPasGTK4.CreateVerticalBox(spacing: Integer): PGtkBox;
begin
  Result := CreateBox(GTK_ORIENTATION_VERTICAL, spacing);
end;

class function TPasGTK4.CreateHorizontalBox(spacing: Integer): PGtkBox;
begin
  Result := CreateBox(GTK_ORIENTATION_HORIZONTAL, spacing);
end;

class procedure TPasGTK4.AddToBox(box: PGtkBox; widget: PGtkWidget);
begin
  gtk_box_append(box, widget);
end;

class procedure TPasGTK4.PrependToBox(box: PGtkBox; widget: PGtkWidget);
begin
  gtk_box_prepend(box, widget);
end;

class function TPasGTK4.CreateGrid: PGtkGrid;
begin
  Result := gtk_grid_new();
end;

class procedure TPasGTK4.AttachToGrid(grid: PGtkGrid; widget: PGtkWidget; left, top, width, height: Integer);
begin
  gtk_grid_attach(grid, widget, left, top, width, height);
end;

// Alignment and margins
class procedure TPasGTK4.SetWidgetAlign(widget: PGtkWidget; halign, valign: GtkAlign);
begin
  gtk_widget_set_halign(widget, halign);
  gtk_widget_set_valign(widget, valign);
end;

class procedure TPasGTK4.SetWidgetMargins(widget: PGtkWidget; top, bottom, start, end_margin: Integer);
begin
  gtk_widget_set_margin_top(widget, top);
  gtk_widget_set_margin_bottom(widget, bottom);
  gtk_widget_set_margin_start(widget, start);
  gtk_widget_set_margin_end(widget, end_margin);
end;

// Signals
class procedure TPasGTK4.ConnectSignal(widget: PGtkWidget; const signal: string; callback: TGtkCallback; data: Pointer);
begin
  if not Assigned(g_signal_connect_data) or not Assigned(callback) or (widget = nil) then
  begin
    WriteLn('ERROR: Invalid parameters for connecting widget signal');
    Exit;
  end;
  g_signal_connect_data(widget, PChar(signal), Pointer(callback), data, nil, 0);
end;

// LibAdwaita methods
class function TPasGTK4.CreateAdwApplication(const app_id: string): PAdwApplication;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ERROR: LibAdwaita not initialized');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_application_new) then
  begin
    WriteLn('ERROR: Function adw_application_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_application_new(PChar(app_id), 0);
    if Result = nil then
      WriteLn('ERROR: adw_application_new returned nil');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating Adwaita application: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateAdwWindow(app: PAdwApplication): PAdwApplicationWindow;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ERROR: LibAdwaita not initialized');
    Result := nil;
    Exit;
  end;
  
  if (app = nil) or not Assigned(adw_application_window_new) then
  begin
    WriteLn('ERROR: Invalid application or function not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_application_window_new(app);
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating Adwaita window: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetAdwWindowContent(window: PAdwApplicationWindow; content: PGtkWidget);
begin
  if not FAdwInitialized or (window = nil) then
  begin
    WriteLn('ERROR: LibAdwaita not initialized or window = nil');
    Exit;
  end;
  
  if not Assigned(adw_application_window_set_content) then
  begin
    WriteLn('ERROR: Function adw_application_window_set_content not loaded');
    Exit;
  end;
  
  try
    adw_application_window_set_content(window, content);
  except
    on E: Exception do
      WriteLn('ERROR while setting Adwaita window content: ', E.Message);
  end;
end;

class function TPasGTK4.CreateAdwHeaderBar: PAdwHeaderBar;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ERROR: LibAdwaita not initialized');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_header_bar_new) then
  begin
    WriteLn('ERROR: Function adw_header_bar_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_header_bar_new();
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating HeaderBar: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetAdwHeaderBarTitle(header_bar: PAdwHeaderBar; title_widget: PGtkWidget);
begin
  if not FAdwInitialized or (header_bar = nil) then
  begin
    WriteLn('ERROR: LibAdwaita not initialized or header_bar = nil');
    Exit;
  end;
  
  if not Assigned(adw_header_bar_set_title_widget) then
  begin
    WriteLn('ERROR: Function adw_header_bar_set_title_widget not loaded');
    Exit;
  end;
  
  try
    adw_header_bar_set_title_widget(header_bar, title_widget);
  except
    on E: Exception do
      WriteLn('ERROR while setting HeaderBar title: ', E.Message);
  end;
end;

class function TPasGTK4.CreateToastOverlay: PAdwToastOverlay;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ERROR: LibAdwaita not initialized');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_toast_overlay_new) then
  begin
    WriteLn('ERROR: Function adw_toast_overlay_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_toast_overlay_new();
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating ToastOverlay: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetToastOverlayChild(overlay: PAdwToastOverlay; child: PGtkWidget);
begin
  if not FAdwInitialized or (overlay = nil) then
  begin
    WriteLn('ERROR: LibAdwaita not initialized or overlay = nil');
    Exit;
  end;
  
  if not Assigned(adw_toast_overlay_set_child) then
  begin
    WriteLn('ERROR: Function adw_toast_overlay_set_child not loaded');
    Exit;
  end;
  
  try
    adw_toast_overlay_set_child(overlay, child);
  except
    on E: Exception do
      WriteLn('ERROR while setting ToastOverlay child: ', E.Message);
  end;
end;

class function TPasGTK4.CreateToast(const title: string): PAdwToast;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ERROR: LibAdwaita not initialized');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_toast_new) then
  begin
    WriteLn('ERROR: Function adw_toast_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_toast_new(PChar(title));
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating Toast: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.ShowToast(overlay: PAdwToastOverlay; toast: PAdwToast);
begin
  if not FAdwInitialized or (overlay = nil) or (toast = nil) then
  begin
    WriteLn('ERROR: LibAdwaita not initialized or parameters = nil');
    Exit;
  end;
  
  if not Assigned(adw_toast_overlay_add_toast) then
  begin
    WriteLn('ERROR: Function adw_toast_overlay_add_toast not loaded');
    Exit;
  end;
  
  try
    adw_toast_overlay_add_toast(overlay, toast);
  except
    on E: Exception do
      WriteLn('ERROR while showing Toast: ', E.Message);
  end;
end;

// Menu methods implementation
class function TPasGTK4.CreateMenuBar: PGtkMenuBar;
begin
  if not Assigned(gtk_menu_bar_new) then
  begin
    WriteLn('ERROR: gtk_menu_bar_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_menu_bar_new();
    if Result = nil then
      WriteLn('ERROR: gtk_menu_bar_new returned nil')
    else
      DebugWriteLn('MenuBar created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating MenuBar: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateMenu: PGtkMenu;
begin
  if not Assigned(gtk_menu_new) then
  begin
    WriteLn('ERROR: gtk_menu_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_menu_new();
    if Result = nil then
      WriteLn('ERROR: gtk_menu_new returned nil')
    else
      DebugWriteLn('Menu created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating Menu: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateMenuItem(const text: string): PGtkMenuItem;
begin
  if not Assigned(gtk_menu_item_new_with_label) then
  begin
    WriteLn('ERROR: gtk_menu_item_new_with_label not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_menu_item_new_with_label(PChar(text));
    if Result = nil then
      WriteLn('ERROR: gtk_menu_item_new_with_label returned nil')
    else
      DebugWriteLn('MenuItem created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating MenuItem: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateMenuItemWithMnemonic(const text: string): PGtkMenuItem;
begin
  if not Assigned(gtk_menu_item_new_with_mnemonic) then
  begin
    WriteLn('ERROR: gtk_menu_item_new_with_mnemonic not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_menu_item_new_with_mnemonic(PChar(text));
    if Result = nil then
      WriteLn('ERROR: gtk_menu_item_new_with_mnemonic returned nil')
    else
      DebugWriteLn('MenuItem with mnemonic created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating MenuItem with mnemonic: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateImageMenuItem(const text: string): PGtkImageMenuItem;
begin
  if not Assigned(gtk_image_menu_item_new_with_label) then
  begin
    WriteLn('ERROR: gtk_image_menu_item_new_with_label not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_image_menu_item_new_with_label(PChar(text));
    if Result = nil then
      WriteLn('ERROR: gtk_image_menu_item_new_with_label returned nil')
    else
      DebugWriteLn('ImageMenuItem created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating ImageMenuItem: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateSeparatorMenuItem: PGtkSeparatorMenuItem;
begin
  if not Assigned(gtk_separator_menu_item_new) then
  begin
    WriteLn('ERROR: gtk_separator_menu_item_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_separator_menu_item_new();
    if Result = nil then
      WriteLn('ERROR: gtk_separator_menu_item_new returned nil')
    else
      DebugWriteLn('SeparatorMenuItem created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating SeparatorMenuItem: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateCheckMenuItem(const text: string): PGtkCheckMenuItem;
begin
  if not Assigned(gtk_check_menu_item_new_with_label) then
  begin
    WriteLn('ERROR: gtk_check_menu_item_new_with_label not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_check_menu_item_new_with_label(PChar(text));
    if Result = nil then
      WriteLn('ERROR: gtk_check_menu_item_new_with_label returned nil')
    else
      DebugWriteLn('CheckMenuItem created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating CheckMenuItem: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateRadioMenuItem(group: Pointer; const text: string): PGtkRadioMenuItem;
begin
  if not Assigned(gtk_radio_menu_item_new_with_label) then
  begin
    WriteLn('ERROR: gtk_radio_menu_item_new_with_label not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_radio_menu_item_new_with_label(group, PChar(text));
    if Result = nil then
      WriteLn('ERROR: gtk_radio_menu_item_new_with_label returned nil')
    else
      DebugWriteLn('RadioMenuItem created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating RadioMenuItem: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetMenuItemSubmenu(menu_item: PGtkMenuItem; submenu: PGtkWidget);
begin
  if (menu_item = nil) or not Assigned(gtk_menu_item_set_submenu) then
  begin
    WriteLn('ERROR: menu_item = nil or gtk_menu_item_set_submenu not loaded');
    Exit;
  end;
  
  try
    gtk_menu_item_set_submenu(menu_item, submenu);
    DebugWriteLn('MenuItem submenu set successfully');
  except
    on E: Exception do
      WriteLn('ERROR while setting MenuItem submenu: ', E.Message);
  end;
end;

class procedure TPasGTK4.AppendToMenuShell(menu_shell: Pointer; item: PGtkWidget);
begin
  if (menu_shell = nil) or (item = nil) or not Assigned(gtk_menu_shell_append) then
  begin
    WriteLn('ERROR: parameters = nil or gtk_menu_shell_append not loaded');
    Exit;
  end;
  
  try
    gtk_menu_shell_append(menu_shell, item);
    DebugWriteLn('Item appended to MenuShell successfully');
  except
    on E: Exception do
      WriteLn('ERROR while appending to MenuShell: ', E.Message);
  end;
end;

class procedure TPasGTK4.PrependToMenuShell(menu_shell: Pointer; item: PGtkWidget);
begin
  if (menu_shell = nil) or (item = nil) or not Assigned(gtk_menu_shell_prepend) then
  begin
    WriteLn('ERROR: parameters = nil or gtk_menu_shell_prepend not loaded');
    Exit;
  end;
  
  try
    gtk_menu_shell_prepend(menu_shell, item);
    DebugWriteLn('Item prepended to MenuShell successfully');
  except
    on E: Exception do
      WriteLn('ERROR while prepending to MenuShell: ', E.Message);
  end;
end;

class procedure TPasGTK4.SetCheckMenuItemActive(check_item: PGtkCheckMenuItem; active: Boolean);
begin
  if (check_item = nil) or not Assigned(gtk_check_menu_item_set_active) then
  begin
    WriteLn('ERROR: check_item = nil or gtk_check_menu_item_set_active not loaded');
    Exit;
  end;
  
  try
    gtk_check_menu_item_set_active(check_item, active);
    DebugWriteLn('CheckMenuItem active state set successfully');
  except
    on E: Exception do
      WriteLn('ERROR while setting CheckMenuItem active state: ', E.Message);
  end;
end;

class function TPasGTK4.GetCheckMenuItemActive(check_item: PGtkCheckMenuItem): Boolean;
begin
  Result := False;
  if (check_item = nil) or not Assigned(gtk_check_menu_item_get_active) then
  begin
    WriteLn('ERROR: check_item = nil or gtk_check_menu_item_get_active not loaded');
    Exit;
  end;
  
  try
    Result := gtk_check_menu_item_get_active(check_item);
  except
    on E: Exception do
    begin
      WriteLn('ERROR while getting CheckMenuItem active state: ', E.Message);
      Result := False;
    end;
  end;
end;

class function TPasGTK4.CreateGMenu: PGMenuModel;
begin
  if not Assigned(g_menu_new) then
  begin
    WriteLn('ERROR: g_menu_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := g_menu_new();
    if Result = nil then
      WriteLn('ERROR: g_menu_new returned nil')
    else
      DebugWriteLn('GMenu created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating GMenu: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.AppendToGMenu(menu: PGMenuModel; const label_text, action: string);
begin
  if (menu = nil) or not Assigned(g_menu_append) then
  begin
    WriteLn('ERROR: menu = nil or g_menu_append not loaded');
    Exit;
  end;
  
  try
    g_menu_append(menu, PChar(label_text), PChar(action));
    DebugWriteLn('Item appended to GMenu successfully');
  except
    on E: Exception do
      WriteLn('ERROR while appending to GMenu: ', E.Message);
  end;
end;

class procedure TPasGTK4.AppendSubmenuToGMenu(menu: PGMenuModel; const label_text: string; submenu: PGMenuModel);
begin
  if (menu = nil) or (submenu = nil) or not Assigned(g_menu_append_submenu) then
  begin
    WriteLn('ERROR: parameters = nil or g_menu_append_submenu not loaded');
    Exit;
  end;
  
  try
    g_menu_append_submenu(menu, PChar(label_text), submenu);
    DebugWriteLn('Submenu appended to GMenu successfully');
  except
    on E: Exception do
      WriteLn('ERROR while appending submenu to GMenu: ', E.Message);
  end;
end;

class function TPasGTK4.CreateSimpleAction(const name: string): PGSimpleAction;
begin
  if not Assigned(g_simple_action_new) then
  begin
    WriteLn('ERROR: g_simple_action_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := g_simple_action_new(PChar(name), nil);
    if Result = nil then
      WriteLn('ERROR: g_simple_action_new returned nil')
    else
      DebugWriteLn('SimpleAction created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating SimpleAction: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.AddActionToMap(action_map: Pointer; action: PGSimpleAction);
begin
  if (action_map = nil) or (action = nil) or not Assigned(g_action_map_add_action) then
  begin
    WriteLn('ERROR: parameters = nil or g_action_map_add_action not loaded');
    Exit;
  end;
  
  try
    g_action_map_add_action(action_map, action);
    DebugWriteLn('Action added to ActionMap successfully');
  except
    on E: Exception do
      WriteLn('ERROR while adding Action to ActionMap: ', E.Message);
  end;
end;

// Modern GTK4 methods implementation
class function TPasGTK4.CreateHeaderBar: PGtkHeaderBar;
begin
  if not Assigned(gtk_header_bar_new) then
  begin
    WriteLn('ERROR: gtk_header_bar_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_header_bar_new();
    if Result = nil then
      WriteLn('ERROR: gtk_header_bar_new returned nil')
    else
      DebugWriteLn('HeaderBar created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating HeaderBar: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetHeaderBarTitle(bar: PGtkHeaderBar; title_widget: PGtkWidget);
begin
  if (bar = nil) or not Assigned(gtk_header_bar_set_title_widget) then
  begin
    WriteLn('ERROR: bar = nil or gtk_header_bar_set_title_widget not loaded');
    Exit;
  end;
  
  try
    gtk_header_bar_set_title_widget(bar, title_widget);
    DebugWriteLn('HeaderBar title set successfully');
  except
    on E: Exception do
      WriteLn('ERROR while setting HeaderBar title: ', E.Message);
  end;
end;

class procedure TPasGTK4.HeaderBarPackStart(bar: PGtkHeaderBar; child: PGtkWidget);
begin
  if (bar = nil) or (child = nil) or not Assigned(gtk_header_bar_pack_start) then
  begin
    WriteLn('ERROR: parameters = nil or gtk_header_bar_pack_start not loaded');
    Exit;
  end;
  
  try
    gtk_header_bar_pack_start(bar, child);
    DebugWriteLn('Widget packed to HeaderBar start');
  except
    on E: Exception do
      WriteLn('ERROR while packing to HeaderBar start: ', E.Message);
  end;
end;

class procedure TPasGTK4.HeaderBarPackEnd(bar: PGtkHeaderBar; child: PGtkWidget);
begin
  if (bar = nil) or (child = nil) or not Assigned(gtk_header_bar_pack_end) then
  begin
    WriteLn('ERROR: parameters = nil or gtk_header_bar_pack_end not loaded');
    Exit;
  end;
  
  try
    gtk_header_bar_pack_end(bar, child);
    DebugWriteLn('Widget packed to HeaderBar end');
  except
    on E: Exception do
      WriteLn('ERROR while packing to HeaderBar end: ', E.Message);
  end;
end;

class function TPasGTK4.CreateMenuButton: PGtkMenuButton;
begin
  if not Assigned(gtk_menu_button_new) then
  begin
    WriteLn('ERROR: gtk_menu_button_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_menu_button_new();
    if Result = nil then
      WriteLn('ERROR: gtk_menu_button_new returned nil')
    else
      DebugWriteLn('MenuButton created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating MenuButton: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetMenuButtonModel(button: PGtkMenuButton; model: PGMenuModel);
begin
  if (button = nil) or not Assigned(gtk_menu_button_set_menu_model) then
  begin
    WriteLn('ERROR: button = nil or gtk_menu_button_set_menu_model not loaded');
    Exit;
  end;
  
  try
    gtk_menu_button_set_menu_model(button, model);
    DebugWriteLn('MenuButton model set successfully');
  except
    on E: Exception do
      WriteLn('ERROR while setting MenuButton model: ', E.Message);
  end;
end;

class function TPasGTK4.CreatePopoverMenu(model: PGMenuModel): PGtkPopoverMenu;
begin
  if not Assigned(gtk_popover_menu_new_from_model) then
  begin
    WriteLn('ERROR: gtk_popover_menu_new_from_model not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_popover_menu_new_from_model(model);
    if Result = nil then
      WriteLn('ERROR: gtk_popover_menu_new_from_model returned nil')
    else
      DebugWriteLn('PopoverMenu created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating PopoverMenu: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetWindowTitlebar(window: PGtkWindow; titlebar: PGtkWidget);
begin
  if (window = nil) or not Assigned(gtk_window_set_titlebar) then
  begin
    WriteLn('ERROR: window = nil or gtk_window_set_titlebar not loaded');
    Exit;
  end;
  
  try
    gtk_window_set_titlebar(window, titlebar);
    DebugWriteLn('Window titlebar set successfully');
  except
    on E: Exception do
      WriteLn('ERROR while setting window titlebar: ', E.Message);
  end;
end;

class function TPasGTK4.CreateSimpleActionGroup: PGSimpleActionGroup;
begin
  if not Assigned(g_simple_action_group_new) then
  begin
    WriteLn('ERROR: g_simple_action_group_new not loaded');
    Result := nil;
    Exit;
  end;
  
  try
    Result := g_simple_action_group_new();
    if Result = nil then
      WriteLn('ERROR: g_simple_action_group_new returned nil')
    else
      DebugWriteLn('SimpleActionGroup created successfully');
  except
    on E: Exception do
    begin
      WriteLn('ERROR while creating SimpleActionGroup: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.InsertActionGroup(widget: PGtkWidget; const name: string; group: PGActionGroup);
begin
  if (widget = nil) or (group = nil) or not Assigned(gtk_widget_insert_action_group) then
  begin
    WriteLn('ERROR: parameters = nil or gtk_widget_insert_action_group not loaded');
    Exit;
  end;
  
  try
    gtk_widget_insert_action_group(widget, PChar(name), group);
    DebugWriteLn('ActionGroup inserted to widget successfully');
  except
    on E: Exception do
      WriteLn('ERROR while inserting ActionGroup: ', E.Message);
  end;
end;

initialization
  TPasGTK4.FLibHandle := 0;
  TPasGTK4.FAdwLibHandle := 0;
  TPasGTK4.FInitialized := False;
  TPasGTK4.FAdwInitialized := False;

finalization
end.