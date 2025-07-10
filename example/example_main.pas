program example_main;

{*
 * PasGTK4 - Pascal bindings for GTK4
 * Example Application
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

// Conditional compilation for debug output
{$IFDEF DEBUG}
  {$DEFINE ENABLE_DEBUG_OUTPUT}
{$ENDIF}

uses
  SysUtils, main, wrapper;


type
  { TExampleApp - Demo application }
  TExampleApp = class(TGTKSimpleWindow)
  private
    FClickCount: Integer;
    FNameEntry: PGtkEntry;
    FResultLabel: PGtkLabel;
    FCounterLabel: PGtkLabel;
    
  public
    constructor Create;
    procedure SetupWindow; override;
    
    // Event handlers
    procedure OnButtonClick(widget: PGtkWidget; data: Pointer);
    procedure OnGreetClick(widget: PGtkWidget; data: Pointer);
    procedure OnClearClick(widget: PGtkWidget; data: Pointer);
  end;
  
  { TAdwaitaExampleApp - Modern application with LibAdwaita }
  TAdwaitaExampleApp = class
  private
    FApp: PAdwApplication;
    FWindow: PAdwApplicationWindow;
    FHeaderBar: PAdwHeaderBar;
    FToastOverlay: PAdwToastOverlay;
    FMainBox: PGtkBox;
    FClickCount: Integer;
    FNameEntry: PGtkEntry;
    FResultLabel: PGtkLabel;
    FCounterLabel: PGtkLabel;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure SetupWindow;
    function Run: Integer;
    
    // Event handlers
    procedure OnActivate(app: PAdwApplication; data: Pointer);
    procedure OnButtonClick(widget: PGtkWidget; data: Pointer);
    procedure OnGreetClick(widget: PGtkWidget; data: Pointer);
    procedure OnClearClick(widget: PGtkWidget; data: Pointer);
    procedure OnToastClick(widget: PGtkWidget; data: Pointer);
  end;

// Global callback functions for event handling
procedure button_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: button_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TExampleApp(data);
      if app <> nil then
        app.OnButtonClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in button_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in button_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in button_click_callback: ', E.Message);
  end;
end;

procedure greet_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: greet_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TExampleApp(data);
      if app <> nil then
        app.OnGreetClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in greet_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in greet_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in greet_click_callback: ', E.Message);
  end;
end;

procedure clear_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: clear_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TExampleApp(data);
      if app <> nil then
        app.OnClearClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in clear_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in clear_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in clear_click_callback: ', E.Message);
  end;
end;

// Adwaita callback functions
procedure adw_app_activate_callback(app: PAdwApplication; data: Pointer); cdecl;
var
  adw_app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (app = nil) then
    begin
      WriteLn('WARNING: adw_app_activate_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      adw_app := TAdwaitaExampleApp(data);
      if adw_app <> nil then
        adw_app.OnActivate(app, data)
      else
        WriteLn('ERROR: Invalid application pointer in adw_app_activate_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in adw_app_activate_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in adw_app_activate_callback: ', E.Message);
  end;
end;

procedure adw_button_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: adw_button_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnButtonClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in adw_button_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in adw_button_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in adw_button_click_callback: ', E.Message);
  end;
end;

procedure adw_greet_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: adw_greet_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnGreetClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in adw_greet_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in adw_greet_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in adw_greet_click_callback: ', E.Message);
  end;
end;

procedure adw_clear_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: adw_clear_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnClearClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in adw_clear_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in adw_clear_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in adw_clear_click_callback: ', E.Message);
  end;
end;

procedure adw_toast_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('WARNING: adw_toast_click_callback called with nil parameters');
      Exit;
    end;
    
    // Safe type checking through try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnToastClick(widget, data)
      else
        WriteLn('ERROR: Invalid application pointer in adw_toast_click_callback');
    except
      on E: Exception do
        WriteLn('ERROR: Invalid type cast in adw_toast_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('CRITICAL ERROR in adw_toast_click_callback: ', E.Message);
  end;
end;


{ TExampleApp }

constructor TExampleApp.Create;
begin
  inherited Create('com.anmitalidev.pasgtk4.example');
  
  FClickCount := 0;
  FNameEntry := nil;
  FResultLabel := nil;
  FCounterLabel := nil;
  
  // Window settings
  Title := 'PasGTK4 Example Application';
  Width := 400;
  Height := 300;
end;

procedure TExampleApp.SetupWindow;
var
  separator_box: PGtkBox;
  button_box: PGtkBox;
  greet_button: PGtkButton;
  clear_button: PGtkButton;
begin
  // Call base setup
  inherited SetupWindow;
  
  // Add title
  AddLabel('Welcome to PasGTK4!');
  
  // Create separator
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 10, 0, 10);
  AddWidget(PGtkWidget(separator_box));
  
  // Click counter
  FCounterLabel := AddLabel('Clicks: 0');
  TPasGTK4.SetLabelJustify(FCounterLabel, GTK_JUSTIFY_CENTER);
  
  // Button for counting clicks
  AddButton('Click me!', @button_click_callback, Self);
  
  // Separator
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 10, 0, 10);
  AddWidget(PGtkWidget(separator_box));
  
  // Name input field
  AddLabel('Enter your name:');
  FNameEntry := AddEntry('Your name');
  
  // Result label
  FResultLabel := AddLabel('');
  TPasGTK4.SetLabelJustify(FResultLabel, GTK_JUSTIFY_CENTER);
  
  // Button container
  button_box := TPasGTK4.CreateHorizontalBox(5);
  TPasGTK4.SetWidgetMargins(PGtkWidget(button_box), 0, 10, 0, 0);
  AddWidget(PGtkWidget(button_box));
  
  // Create action buttons separately
  greet_button := TPasGTK4.CreateButton('Greet');
  TPasGTK4.ConnectSignal(PGtkWidget(greet_button), 'clicked', @greet_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(greet_button));
  
  clear_button := TPasGTK4.CreateButton('Clear');
  TPasGTK4.ConnectSignal(PGtkWidget(clear_button), 'clicked', @clear_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(clear_button));
end;

procedure TExampleApp.OnButtonClick(widget: PGtkWidget; data: Pointer);
begin
  // Check if widget is created
  if FCounterLabel = nil then
  begin
    WriteLn('ERROR: FCounterLabel not created');
    Exit;
  end;
  
  Inc(FClickCount);
  TPasGTK4.SetLabelText(FCounterLabel, 'Clicks: ' + IntToStr(FClickCount));
  
  WriteLn('Button clicked! Total clicks: ', FClickCount);
end;

procedure TExampleApp.OnGreetClick(widget: PGtkWidget; data: Pointer);
var
  name: string;
  greeting: string;
begin
{$IFDEF ENABLE_DEBUG_OUTPUT}
  WriteLn('[DEBUG] FNameEntry = ', PtrUInt(FNameEntry));
{$ENDIF}
  
  // Check if widgets are created
  if FNameEntry = nil then
  begin
    WriteLn('ERROR: FNameEntry not created');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ERROR: FResultLabel not created');
    Exit;
  end;
  
  name := TPasGTK4.GetEntryText(FNameEntry);
  
  if Trim(name) = '' then
    greeting := 'Hello, stranger!'
  else
    greeting := 'Hello, ' + name + '! Welcome to PasGTK4!';
  
  TPasGTK4.SetLabelText(FResultLabel, greeting);
  
  WriteLn('Greeting: ', greeting);
end;

procedure TExampleApp.OnClearClick(widget: PGtkWidget; data: Pointer);
begin
  // Check if widgets are created
  if FNameEntry = nil then
  begin
    WriteLn('ERROR: FNameEntry not created');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ERROR: FResultLabel not created');
    Exit;
  end;
  
  if FCounterLabel = nil then
  begin
    WriteLn('ERROR: FCounterLabel not created');
    Exit;
  end;
  
  TPasGTK4.SetEntryText(FNameEntry, '');
  TPasGTK4.SetLabelText(FResultLabel, '');
  FClickCount := 0;
  TPasGTK4.SetLabelText(FCounterLabel, 'Clicks: 0');
  
  WriteLn('Data cleared');
end;

{ TAdwaitaExampleApp }

constructor TAdwaitaExampleApp.Create;
begin
  inherited Create;
  
  FClickCount := 0;
  FApp := nil;
  FWindow := nil;
  FHeaderBar := nil;
  FToastOverlay := nil;
  FMainBox := nil;
  FNameEntry := nil;
  FResultLabel := nil;
  FCounterLabel := nil;
  
  // Create Adwaita application
  FApp := TPasGTK4.CreateAdwApplication('com.anmitalidev.pasgtk4.adwaita-example');
  if FApp = nil then
    raise Exception.Create('Failed to create Adwaita application');
  
  // Connect activation handler
  TPasGTK4.ConnectApplicationSignal(PGtkApplication(FApp), 'activate', @adw_app_activate_callback, Self);
end;

destructor TAdwaitaExampleApp.Destroy;
begin
  inherited Destroy;
end;

procedure TAdwaitaExampleApp.OnActivate(app: PAdwApplication; data: Pointer);
begin
  // Create Adwaita window
  FWindow := TPasGTK4.CreateAdwWindow(FApp);
  if FWindow = nil then
  begin
    WriteLn('ERROR: Failed to create Adwaita window');
    Exit;
  end;
  
  // Set window size
  TPasGTK4.SetWindowSize(PGtkWindow(FWindow), 500, 400);
  
  // Setup interface
  SetupWindow;
  
  // Show window
  TPasGTK4.ShowWindow(PGtkWindow(FWindow));
end;

procedure TAdwaitaExampleApp.SetupWindow;
var
  title_label: PGtkLabel;
  separator_box: PGtkBox;
  button_box: PGtkBox;
  greet_button: PGtkButton;
  clear_button: PGtkButton;
  toast_button: PGtkButton;
  click_button: PGtkButton;
begin
  // Create HeaderBar (modern title bar)
  FHeaderBar := TPasGTK4.CreateHeaderBar;
  
  // Create ToastOverlay (for notifications)
  FToastOverlay := TPasGTK4.CreateToastOverlay;
  if FToastOverlay = nil then
  begin
    WriteLn('ERROR: Failed to create ToastOverlay');
    Exit;
  end;
  
  // Create main container
  FMainBox := TPasGTK4.CreateVerticalBox(12);
  TPasGTK4.SetWidgetMargins(PGtkWidget(FMainBox), 24, 24, 24, 24);
  
  // Set main container in ToastOverlay
  TPasGTK4.SetToastOverlayChild(FToastOverlay, PGtkWidget(FMainBox));
  
  // If there's HeaderBar, create title
  if FHeaderBar <> nil then
  begin
    title_label := TPasGTK4.CreateLabel('PasGTK4 with LibAdwaita');
    TPasGTK4.SetHeaderBarTitle(FHeaderBar, PGtkWidget(title_label));
  end;
  
  // Set window content (for AdwApplicationWindow)
  TPasGTK4.SetAdwWindowContent(FWindow, PGtkWidget(FToastOverlay));
  
  // Add welcome text
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(TPasGTK4.CreateLabel('Welcome to modern PasGTK4!')));
  
  // Create separator
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 12, 0, 12);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(separator_box));
  
  // Click counter
  FCounterLabel := TPasGTK4.CreateLabel('Clicks: 0');
  TPasGTK4.SetLabelJustify(FCounterLabel, GTK_JUSTIFY_CENTER);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(FCounterLabel));
  
  // Button for counting clicks
  click_button := TPasGTK4.CreateButton('Click me!');
  TPasGTK4.ConnectSignal(PGtkWidget(click_button), 'clicked', @adw_button_click_callback, Self);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(click_button));
  
  // Separator
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 12, 0, 12);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(separator_box));
  
  // Name input field
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(TPasGTK4.CreateLabel('Enter your name:')));
  FNameEntry := TPasGTK4.CreateEntry;
  TPasGTK4.SetEntryPlaceholder(FNameEntry, 'Your name');
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(FNameEntry));
  
  // Result label
  FResultLabel := TPasGTK4.CreateLabel('');
  TPasGTK4.SetLabelJustify(FResultLabel, GTK_JUSTIFY_CENTER);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(FResultLabel));
  
  // Button container
  button_box := TPasGTK4.CreateHorizontalBox(8);
  TPasGTK4.SetWidgetMargins(PGtkWidget(button_box), 0, 12, 0, 0);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(button_box));
  
  // Create action buttons
  greet_button := TPasGTK4.CreateButton('Greet');
  TPasGTK4.ConnectSignal(PGtkWidget(greet_button), 'clicked', @adw_greet_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(greet_button));
  
  clear_button := TPasGTK4.CreateButton('Clear');
  TPasGTK4.ConnectSignal(PGtkWidget(clear_button), 'clicked', @adw_clear_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(clear_button));
  
  toast_button := TPasGTK4.CreateButton('Toast');
  TPasGTK4.ConnectSignal(PGtkWidget(toast_button), 'clicked', @adw_toast_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(toast_button));
end;

function TAdwaitaExampleApp.Run: Integer;
begin
  Result := TPasGTK4.RunApplication(PGtkApplication(FApp));
end;

procedure TAdwaitaExampleApp.OnButtonClick(widget: PGtkWidget; data: Pointer);
begin
  // Check if widget is created
  if FCounterLabel = nil then
  begin
    WriteLn('ERROR: FCounterLabel not created');
    Exit;
  end;
  
  Inc(FClickCount);
  TPasGTK4.SetLabelText(FCounterLabel, 'Clicks: ' + IntToStr(FClickCount));
  WriteLn('Button clicked! Total clicks: ', FClickCount);
end;

procedure TAdwaitaExampleApp.OnGreetClick(widget: PGtkWidget; data: Pointer);
var
  name: string;
  greeting: string;
begin
  // Check if widgets are created
  if FNameEntry = nil then
  begin
    WriteLn('ERROR: FNameEntry not created');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ERROR: FResultLabel not created');
    Exit;
  end;
  
  name := TPasGTK4.GetEntryText(FNameEntry);
  
  if Trim(name) = '' then
    greeting := 'Hello, stranger!'
  else
    greeting := 'Hello, ' + name + '! Welcome to modern PasGTK4!';
  
  TPasGTK4.SetLabelText(FResultLabel, greeting);
  WriteLn('Greeting: ', greeting);
end;

procedure TAdwaitaExampleApp.OnClearClick(widget: PGtkWidget; data: Pointer);
begin
  // Check if widgets are created
  if FNameEntry = nil then
  begin
    WriteLn('ERROR: FNameEntry not created');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ERROR: FResultLabel not created');
    Exit;
  end;
  
  if FCounterLabel = nil then
  begin
    WriteLn('ERROR: FCounterLabel not created');
    Exit;
  end;
  
  TPasGTK4.SetEntryText(FNameEntry, '');
  TPasGTK4.SetLabelText(FResultLabel, '');
  FClickCount := 0;
  TPasGTK4.SetLabelText(FCounterLabel, 'Clicks: 0');
  WriteLn('Data cleared');
end;

procedure TAdwaitaExampleApp.OnToastClick(widget: PGtkWidget; data: Pointer);
var
  toast: PAdwToast;
begin
  // Check if ToastOverlay is created
  if FToastOverlay = nil then
  begin
    WriteLn('ERROR: FToastOverlay not created');
    Exit;
  end;
  
  toast := TPasGTK4.CreateToast('Modern LibAdwaita notification!');
  if toast = nil then
  begin
    WriteLn('ERROR: Failed to create Toast');
    Exit;
  end;
  
  TPasGTK4.ShowToast(FToastOverlay, toast);
  WriteLn('Toast notification shown');
end;

var
  app: TExampleApp;
  adw_app: TAdwaitaExampleApp;
  result_code: Integer;
  use_adwaita: Boolean;

begin
  WriteLn('PasGTK4 Example Application');
  WriteLn('Version: ', GetPasGTK4Version);
  WriteLn('Author: AnmiTaliDev');
  WriteLn('License: Apache 2.0');
  WriteLn('---');
  
  // Check command line parameters
  use_adwaita := (ParamCount > 0) and (ParamStr(1) = '--adwaita');
  
  if use_adwaita then
  begin
    WriteLn('Mode: LibAdwaita (modern GNOME design)');
    WriteLn('');
    
    // Initialize PasGTK4 with LibAdwaita
    if not InitializePasGTK4WithAdwaita then
    begin
      WriteLn('ERROR: Failed to initialize PasGTK4 with LibAdwaita');
      WriteLn('Make sure GTK4 and LibAdwaita are installed on your system');
      Halt(1);
    end;
    
    WriteLn('PasGTK4 with LibAdwaita successfully initialized');
    
    try
      // Create and run Adwaita application
      adw_app := TAdwaitaExampleApp.Create;
      try
        WriteLn('Starting modern application...');
        result_code := adw_app.Run;
        WriteLn('Application finished with code: ', result_code);
      finally
        adw_app.Free;
      end;
    except
      on E: Exception do
      begin
        WriteLn('ERROR: ', E.Message);
        Halt(1);
      end;
    end;
  end
  else
  begin
    WriteLn('Mode: Regular GTK4');
    WriteLn('Run with --adwaita parameter for modern design');
    WriteLn('');
    
    // Initialize PasGTK4
    if not InitializePasGTK4 then
    begin
      WriteLn('ERROR: Failed to initialize PasGTK4');
      WriteLn('Make sure GTK4 is installed on your system');
      Halt(1);
    end;
    
    WriteLn('PasGTK4 successfully initialized');
    
    try
      // Create and run regular application
      app := TExampleApp.Create;
      try
        WriteLn('Starting application...');
        result_code := app.Run;
        WriteLn('Application finished with code: ', result_code);
      finally
        app.Free;
      end;
    except
      on E: Exception do
      begin
        WriteLn('ERROR: ', E.Message);
        Halt(1);
      end;
    end;
  end;
  
  // Finalize PasGTK4
  FinalizePasGTK4;
  WriteLn('PasGTK4 finalized');
end.