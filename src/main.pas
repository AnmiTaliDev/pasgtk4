unit main;

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

end.