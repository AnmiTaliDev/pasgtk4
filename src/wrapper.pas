unit wrapper;

{*
 * PasGTK4 - Pascal bindings for GTK4
 * Copyright (C) 2025 AnmiTaliDev
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *}

{$mode objfpc}{$H+}

// Условная компиляция отладочного вывода
{$IFDEF DEBUG}
  {$DEFINE ENABLE_DEBUG_OUTPUT}
{$ENDIF}

interface

uses
  ctypes, dynlibs, SysUtils, Math;

const
  PASGTK4_VERSION = '1.0.0';

{$IFDEF ENABLE_DEBUG_OUTPUT}
  // Отладочный вывод включен
  procedure DebugWriteLn(const msg: string);
{$ELSE}
  // Пустая заглушка для release сборки
  procedure DebugWriteLn(const msg: string);
{$ENDIF}

type
  // GTK4 основные типы
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
  
  // LibAdwaita types
  PAdwApplication = Pointer;
  PAdwApplicationWindow = Pointer;
  PAdwHeaderBar = Pointer;
  PAdwToastOverlay = Pointer;
  PAdwToast = Pointer;
  
  // Перечисления
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
  
  // Callback типы
  TGtkCallback = procedure(widget: PGtkWidget; data: Pointer); cdecl;
  TGtkApplicationCallback = procedure(app: PGtkApplication; data: Pointer); cdecl;
  
  // Класс для работы с GTK4
  TPasGTK4 = class
  private
    class var FLibHandle: TLibHandle;
    class var FAdwLibHandle: TLibHandle;
    class var FInitialized: Boolean;
    class var FAdwInitialized: Boolean;
    
    // Указатели на функции GTK4
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
    // Инициализация и финализация
    class function Initialize: Boolean;
    class function InitializeAdwaita: Boolean;
    class procedure Finalize;
    class function IsInitialized: Boolean;
    class function IsAdwaitaInitialized: Boolean;
    class function GetVersion: string;
    
    // Приложение
    class function CreateApplication(const app_id: string): PGtkApplication;
    class function RunApplication(app: PGtkApplication): Integer;
    class procedure ConnectApplicationSignal(app: PGtkApplication; const signal: string; callback: TGtkApplicationCallback; data: Pointer = nil);
    
    // Окна
    class function CreateWindow(app: PGtkApplication): PGtkWindow;
    class procedure SetWindowTitle(window: PGtkWindow; const title: string);
    class procedure SetWindowSize(window: PGtkWindow; width, height: Integer);
    class procedure SetWindowChild(window: PGtkWindow; child: PGtkWidget);
    class procedure ShowWindow(window: PGtkWindow);
    
    // Виджеты
    class function CreateButton(const text: string): PGtkButton;
    class function CreateLabel(const text: string): PGtkLabel;
    class procedure SetLabelText(label_widget: PGtkLabel; const text: string);
    class procedure SetLabelJustify(label_widget: PGtkLabel; justify: GtkJustification);
    class function CreateEntry: PGtkEntry;
    class procedure SetEntryText(entry: PGtkEntry; const text: string);
    class function GetEntryText(entry: PGtkEntry): string;
    class procedure SetEntryPlaceholder(entry: PGtkEntry; const text: string);
    
    // Контейнеры
    class function CreateBox(orientation: GtkOrientation; spacing: Integer): PGtkBox;
    class function CreateVerticalBox(spacing: Integer): PGtkBox;
    class function CreateHorizontalBox(spacing: Integer): PGtkBox;
    class procedure AddToBox(box: PGtkBox; widget: PGtkWidget);
    class procedure PrependToBox(box: PGtkBox; widget: PGtkWidget);
    class function CreateGrid: PGtkGrid;
    class procedure AttachToGrid(grid: PGtkGrid; widget: PGtkWidget; left, top, width, height: Integer);
    
    // Выравнивание и отступы
    class procedure SetWidgetAlign(widget: PGtkWidget; halign, valign: GtkAlign);
    class procedure SetWidgetMargins(widget: PGtkWidget; top, bottom, start, end_margin: Integer);
    
    // Сигналы
    class procedure ConnectSignal(widget: PGtkWidget; const signal: string; callback: TGtkCallback; data: Pointer = nil);
    
    // LibAdwaita методы
    class function CreateAdwApplication(const app_id: string): PAdwApplication;
    class function CreateAdwWindow(app: PAdwApplication): PAdwApplicationWindow;
    class procedure SetAdwWindowContent(window: PAdwApplicationWindow; content: PGtkWidget);
    class function CreateHeaderBar: PAdwHeaderBar;
    class procedure SetHeaderBarTitle(header_bar: PAdwHeaderBar; title_widget: PGtkWidget);
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
  // Ничего не делаем в release сборке
end;
{$ENDIF}

class function TPasGTK4.LoadLibrary: Boolean;
const
  GTK_LIBS: array[0..4] of string = (
    'libgtk-4.so.1',      // Linux
    'libgtk-4.so',        // Linux альтернативный
    'libgtk-4.dylib',     // macOS
    'gtk-4.dll',          // Windows
    'libgtk-4-1.dll'      // Windows альтернативный
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
    'libadwaita-1.so',      // Linux альтернативный
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
  
  DebugWriteLn('Загружаем GTK4 библиотеку...');
  if not LoadLibrary then
  begin
    WriteLn('ОШИБКА: Не удалось загрузить GTK4 библиотеку');
    Exit;
  end;
  
  DebugWriteLn('Загружаем функции GTK4...');
  // Загружаем указатели на функции
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
  
  // Проверяем основные функции
  if not (Assigned(gtk_application_new) and 
          Assigned(gtk_application_window_new) and Assigned(gtk_window_set_title) and
          Assigned(g_signal_connect_data) and Assigned(g_application_run)) then
  begin
    WriteLn('ОШИБКА: Не удалось загрузить все необходимые функции GTK4');
    Exit;
  end;
  
  // Инициализируем GTK4 если функция доступна
  if Assigned(gtk_init_check) then
  begin
    DebugWriteLn('Инициализируем GTK4...');
    if not gtk_init_check() then
    begin
      WriteLn('ОШИБКА: Не удалось инициализировать GTK4');
      Exit;
    end;
    DebugWriteLn('GTK4 инициализирован успешно');
  end else
    DebugWriteLn('GTK4 готов к использованию');
  
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
    WriteLn('ОШИБКА: GTK4 должен быть инициализирован перед LibAdwaita');
    Exit;
  end;
  
  DebugWriteLn('Загружаем LibAdwaita библиотеку...');
  if not LoadAdwaitaLibrary then
  begin
    WriteLn('ОШИБКА: Не удалось загрузить LibAdwaita библиотеку');
    Exit;
  end;
  
  DebugWriteLn('Загружаем функции LibAdwaita...');
  // Загружаем указатели на функции LibAdwaita
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
  
  // Проверяем основные функции
  if not (Assigned(adw_init) and Assigned(adw_application_new) and 
          Assigned(adw_application_window_new) and Assigned(adw_header_bar_new)) then
  begin
    WriteLn('ОШИБКА: Не удалось загрузить все необходимые функции LibAdwaita');
    Exit;
  end;
  
  // Инициализируем LibAdwaita
  DebugWriteLn('Инициализируем LibAdwaita...');
  try
    adw_init();
    DebugWriteLn('LibAdwaita инициализирован успешно');
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при инициализации LibAdwaita: ', E.Message);
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

// Приложение
class function TPasGTK4.CreateApplication(const app_id: string): PGtkApplication;
begin
  if not Assigned(gtk_application_new) then
  begin
    WriteLn('ОШИБКА: Функция gtk_application_new не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_application_new(PChar(app_id), 0);
    if Result = nil then
      WriteLn('ОШИБКА: gtk_application_new вернула nil');
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании приложения: ', E.Message);
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
    WriteLn('ОШИБКА: Приложение не создано');
    Result := -1;
    Exit;
  end;
  
  if not Assigned(g_application_run) then
  begin
    WriteLn('ОШИБКА: Функция g_application_run не загружена');
    Result := -1;
    Exit;
  end;
  
  // Маскируем исключения FPU, которые могут возникать в GTK4
  oldFPUMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  
  try
    Result := g_application_run(app, 0, nil);
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при запуске приложения: ', E.Message);
      Result := -1;
    end;
  end;
  
  // Восстанавливаем маску FPU
  SetExceptionMask(oldFPUMask);
end;

class procedure TPasGTK4.ConnectApplicationSignal(app: PGtkApplication; const signal: string; callback: TGtkApplicationCallback; data: Pointer);
begin
  if not Assigned(g_signal_connect_data) or not Assigned(callback) or (app = nil) then
  begin
    WriteLn('ОШИБКА: Некорректные параметры для подключения сигнала приложения');
    Exit;
  end;
  g_signal_connect_data(app, PChar(signal), Pointer(callback), data, nil, 0);
end;

// Окна
class function TPasGTK4.CreateWindow(app: PGtkApplication): PGtkWindow;
begin
  Result := gtk_application_window_new(app);
end;

class procedure TPasGTK4.SetWindowTitle(window: PGtkWindow; const title: string);
begin
  if (window = nil) or not Assigned(gtk_window_set_title) then
  begin
    WriteLn('ОШИБКА: window = nil или функция gtk_window_set_title не загружена');
    Exit;
  end;
  gtk_window_set_title(window, PChar(title));
end;

class procedure TPasGTK4.SetWindowSize(window: PGtkWindow; width, height: Integer);
begin
  if (window = nil) or not Assigned(gtk_window_set_default_size) then
  begin
    WriteLn('ОШИБКА: window = nil или функция gtk_window_set_default_size не загружена');
    Exit;
  end;
  gtk_window_set_default_size(window, width, height);
end;

class procedure TPasGTK4.SetWindowChild(window: PGtkWindow; child: PGtkWidget);
begin
  if (window = nil) or not Assigned(gtk_window_set_child) then
  begin
    WriteLn('ОШИБКА: window = nil или функция gtk_window_set_child не загружена');
    Exit;
  end;
  gtk_window_set_child(window, child);
end;

class procedure TPasGTK4.ShowWindow(window: PGtkWindow);
begin
  if (window = nil) or not Assigned(gtk_window_present) then
  begin
    WriteLn('ОШИБКА: window = nil или функция gtk_window_present не загружена');
    Exit;
  end;
  gtk_window_present(window);
end;

// Виджеты
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
    WriteLn('ОШИБКА: gtk_entry_new не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := gtk_entry_new();
    if Result = nil then
      WriteLn('ОШИБКА: gtk_entry_new вернула nil')
    else
      DebugWriteLn('Entry создан успешно');
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании entry: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetEntryText(entry: PGtkEntry; const text: string);
begin
  if entry = nil then
  begin
    WriteLn('ОШИБКА: entry = nil в SetEntryText');
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
      WriteLn('ОШИБКА: ни gtk_editable_set_text, ни gtk_entry_set_text не загружены');
  except
    on E: Exception do
      WriteLn('ОШИБКА при установке текста entry: ', E.Message);
  end;
end;

class function TPasGTK4.GetEntryText(entry: PGtkEntry): string;
var
  text_ptr: PChar;
begin
  Result := '';
  if entry = nil then
  begin
    WriteLn('ОШИБКА: entry = nil');
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
      WriteLn('ОШИБКА: ни gtk_editable_get_text, ни gtk_entry_get_text не загружены');
      Exit;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при получении текста entry: ', E.Message);
      Result := '';
    end;
  end;
end;

class procedure TPasGTK4.SetEntryPlaceholder(entry: PGtkEntry; const text: string);
begin
  gtk_entry_set_placeholder_text(entry, PChar(text));
end;

// Контейнеры
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

// Выравнивание и отступы
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

// Сигналы
class procedure TPasGTK4.ConnectSignal(widget: PGtkWidget; const signal: string; callback: TGtkCallback; data: Pointer);
begin
  if not Assigned(g_signal_connect_data) or not Assigned(callback) or (widget = nil) then
  begin
    WriteLn('ОШИБКА: Некорректные параметры для подключения сигнала виджета');
    Exit;
  end;
  g_signal_connect_data(widget, PChar(signal), Pointer(callback), data, nil, 0);
end;

// LibAdwaita методы
class function TPasGTK4.CreateAdwApplication(const app_id: string): PAdwApplication;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_application_new) then
  begin
    WriteLn('ОШИБКА: Функция adw_application_new не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_application_new(PChar(app_id), 0);
    if Result = nil then
      WriteLn('ОШИБКА: adw_application_new вернула nil');
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании Adwaita приложения: ', E.Message);
      Result := nil;
    end;
  end;
end;

class function TPasGTK4.CreateAdwWindow(app: PAdwApplication): PAdwApplicationWindow;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован');
    Result := nil;
    Exit;
  end;
  
  if (app = nil) or not Assigned(adw_application_window_new) then
  begin
    WriteLn('ОШИБКА: Некорректное приложение или функция не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_application_window_new(app);
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании Adwaita окна: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetAdwWindowContent(window: PAdwApplicationWindow; content: PGtkWidget);
begin
  if not FAdwInitialized or (window = nil) then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован или window = nil');
    Exit;
  end;
  
  if not Assigned(adw_application_window_set_content) then
  begin
    WriteLn('ОШИБКА: Функция adw_application_window_set_content не загружена');
    Exit;
  end;
  
  try
    adw_application_window_set_content(window, content);
  except
    on E: Exception do
      WriteLn('ОШИБКА при установке содержимого Adwaita окна: ', E.Message);
  end;
end;

class function TPasGTK4.CreateHeaderBar: PAdwHeaderBar;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_header_bar_new) then
  begin
    WriteLn('ОШИБКА: Функция adw_header_bar_new не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_header_bar_new();
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании HeaderBar: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetHeaderBarTitle(header_bar: PAdwHeaderBar; title_widget: PGtkWidget);
begin
  if not FAdwInitialized or (header_bar = nil) then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован или header_bar = nil');
    Exit;
  end;
  
  if not Assigned(adw_header_bar_set_title_widget) then
  begin
    WriteLn('ОШИБКА: Функция adw_header_bar_set_title_widget не загружена');
    Exit;
  end;
  
  try
    adw_header_bar_set_title_widget(header_bar, title_widget);
  except
    on E: Exception do
      WriteLn('ОШИБКА при установке заголовка HeaderBar: ', E.Message);
  end;
end;

class function TPasGTK4.CreateToastOverlay: PAdwToastOverlay;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_toast_overlay_new) then
  begin
    WriteLn('ОШИБКА: Функция adw_toast_overlay_new не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_toast_overlay_new();
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании ToastOverlay: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.SetToastOverlayChild(overlay: PAdwToastOverlay; child: PGtkWidget);
begin
  if not FAdwInitialized or (overlay = nil) then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован или overlay = nil');
    Exit;
  end;
  
  if not Assigned(adw_toast_overlay_set_child) then
  begin
    WriteLn('ОШИБКА: Функция adw_toast_overlay_set_child не загружена');
    Exit;
  end;
  
  try
    adw_toast_overlay_set_child(overlay, child);
  except
    on E: Exception do
      WriteLn('ОШИБКА при установке дочернего элемента ToastOverlay: ', E.Message);
  end;
end;

class function TPasGTK4.CreateToast(const title: string): PAdwToast;
begin
  if not FAdwInitialized then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован');
    Result := nil;
    Exit;
  end;
  
  if not Assigned(adw_toast_new) then
  begin
    WriteLn('ОШИБКА: Функция adw_toast_new не загружена');
    Result := nil;
    Exit;
  end;
  
  try
    Result := adw_toast_new(PChar(title));
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА при создании Toast: ', E.Message);
      Result := nil;
    end;
  end;
end;

class procedure TPasGTK4.ShowToast(overlay: PAdwToastOverlay; toast: PAdwToast);
begin
  if not FAdwInitialized or (overlay = nil) or (toast = nil) then
  begin
    WriteLn('ОШИБКА: LibAdwaita не инициализирован или параметры = nil');
    Exit;
  end;
  
  if not Assigned(adw_toast_overlay_add_toast) then
  begin
    WriteLn('ОШИБКА: Функция adw_toast_overlay_add_toast не загружена');
    Exit;
  end;
  
  try
    adw_toast_overlay_add_toast(overlay, toast);
  except
    on E: Exception do
      WriteLn('ОШИБКА при показе Toast: ', E.Message);
  end;
end;

initialization
  TPasGTK4.FLibHandle := 0;
  TPasGTK4.FAdwLibHandle := 0;
  TPasGTK4.FInitialized := False;
  TPasGTK4.FAdwInitialized := False;

finalization
  // Автоматически освобождаем ресурсы при завершении программы
  TPasGTK4.Finalize;

end.