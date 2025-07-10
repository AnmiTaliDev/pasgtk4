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

// Условная компиляция отладочного вывода
{$IFDEF DEBUG}
  {$DEFINE ENABLE_DEBUG_OUTPUT}
{$ENDIF}

uses
  SysUtils, main, wrapper;


type
  { TExampleApp - Демонстрационное приложение }
  TExampleApp = class(TGTKSimpleWindow)
  private
    FClickCount: Integer;
    FNameEntry: PGtkEntry;
    FResultLabel: PGtkLabel;
    FCounterLabel: PGtkLabel;
    
  public
    constructor Create;
    procedure SetupWindow; override;
    
    // Обработчики событий
    procedure OnButtonClick(widget: PGtkWidget; data: Pointer);
    procedure OnGreetClick(widget: PGtkWidget; data: Pointer);
    procedure OnClearClick(widget: PGtkWidget; data: Pointer);
  end;
  
  { TAdwaitaExampleApp - Современное приложение с LibAdwaita }
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
    
    // Обработчики событий
    procedure OnActivate(app: PAdwApplication; data: Pointer);
    procedure OnButtonClick(widget: PGtkWidget; data: Pointer);
    procedure OnGreetClick(widget: PGtkWidget; data: Pointer);
    procedure OnClearClick(widget: PGtkWidget; data: Pointer);
    procedure OnToastClick(widget: PGtkWidget; data: Pointer);
  end;

// Глобальные callback-функции для обработки событий
procedure button_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: button_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TExampleApp(data);
      if app <> nil then
        app.OnButtonClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в button_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в button_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в button_click_callback: ', E.Message);
  end;
end;

procedure greet_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: greet_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TExampleApp(data);
      if app <> nil then
        app.OnGreetClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в greet_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в greet_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в greet_click_callback: ', E.Message);
  end;
end;

procedure clear_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: clear_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TExampleApp(data);
      if app <> nil then
        app.OnClearClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в clear_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в clear_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в clear_click_callback: ', E.Message);
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
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: adw_app_activate_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      adw_app := TAdwaitaExampleApp(data);
      if adw_app <> nil then
        adw_app.OnActivate(app, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в adw_app_activate_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в adw_app_activate_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в adw_app_activate_callback: ', E.Message);
  end;
end;

procedure adw_button_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: adw_button_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnButtonClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в adw_button_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в adw_button_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в adw_button_click_callback: ', E.Message);
  end;
end;

procedure adw_greet_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: adw_greet_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnGreetClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в adw_greet_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в adw_greet_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в adw_greet_click_callback: ', E.Message);
  end;
end;

procedure adw_clear_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: adw_clear_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnClearClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в adw_clear_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в adw_clear_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в adw_clear_click_callback: ', E.Message);
  end;
end;

procedure adw_toast_click_callback(widget: PGtkWidget; data: Pointer); cdecl;
var
  app: TAdwaitaExampleApp;
begin
  try
    if (data = nil) or (widget = nil) then
    begin
      WriteLn('ПРЕДУПРЕЖДЕНИЕ: adw_toast_click_callback вызван с nil параметрами');
      Exit;
    end;
    
    // Безопасная проверка типа через try-except
    try
      app := TAdwaitaExampleApp(data);
      if app <> nil then
        app.OnToastClick(widget, data)
      else
        WriteLn('ОШИБКА: Некорректный указатель приложения в adw_toast_click_callback');
    except
      on E: Exception do
        WriteLn('ОШИБКА: Некорректное приведение типа в adw_toast_click_callback: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('КРИТИЧЕСКАЯ ОШИБКА в adw_toast_click_callback: ', E.Message);
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
  
  // Настройки окна
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
  // Вызываем базовую настройку
  inherited SetupWindow;
  
  // Добавляем заголовок
  AddLabel('Добро пожаловать в PasGTK4!');
  
  // Создаем разделитель
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 10, 0, 10);
  AddWidget(PGtkWidget(separator_box));
  
  // Счетчик кликов
  FCounterLabel := AddLabel('Кликов: 0');
  TPasGTK4.SetLabelJustify(FCounterLabel, GTK_JUSTIFY_CENTER);
  
  // Кнопка для подсчета кликов
  AddButton('Нажми меня!', @button_click_callback, Self);
  
  // Разделитель
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 10, 0, 10);
  AddWidget(PGtkWidget(separator_box));
  
  // Поле ввода имени
  AddLabel('Введите ваше имя:');
  FNameEntry := AddEntry('Ваше имя');
  
  // Метка для результата
  FResultLabel := AddLabel('');
  TPasGTK4.SetLabelJustify(FResultLabel, GTK_JUSTIFY_CENTER);
  
  // Контейнер для кнопок
  button_box := TPasGTK4.CreateHorizontalBox(5);
  TPasGTK4.SetWidgetMargins(PGtkWidget(button_box), 0, 10, 0, 0);
  AddWidget(PGtkWidget(button_box));
  
  // Создаем кнопки действий отдельно
  greet_button := TPasGTK4.CreateButton('Поздороваться');
  TPasGTK4.ConnectSignal(PGtkWidget(greet_button), 'clicked', @greet_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(greet_button));
  
  clear_button := TPasGTK4.CreateButton('Очистить');
  TPasGTK4.ConnectSignal(PGtkWidget(clear_button), 'clicked', @clear_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(clear_button));
end;

procedure TExampleApp.OnButtonClick(widget: PGtkWidget; data: Pointer);
begin
  // Проверяем, что виджет создан
  if FCounterLabel = nil then
  begin
    WriteLn('ОШИБКА: FCounterLabel не создан');
    Exit;
  end;
  
  Inc(FClickCount);
  TPasGTK4.SetLabelText(FCounterLabel, 'Кликов: ' + IntToStr(FClickCount));
  
  WriteLn('Кнопка нажата! Всего кликов: ', FClickCount);
end;

procedure TExampleApp.OnGreetClick(widget: PGtkWidget; data: Pointer);
var
  name: string;
  greeting: string;
begin
{$IFDEF ENABLE_DEBUG_OUTPUT}
  WriteLn('[DEBUG] FNameEntry = ', PtrUInt(FNameEntry));
{$ENDIF}
  
  // Проверяем, что виджеты созданы
  if FNameEntry = nil then
  begin
    WriteLn('ОШИБКА: FNameEntry не создан');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ОШИБКА: FResultLabel не создан');
    Exit;
  end;
  
  name := TPasGTK4.GetEntryText(FNameEntry);
  
  if Trim(name) = '' then
    greeting := 'Привет, незнакомец!'
  else
    greeting := 'Привет, ' + name + '! Добро пожаловать в PasGTK4!';
  
  TPasGTK4.SetLabelText(FResultLabel, greeting);
  
  WriteLn('Приветствие: ', greeting);
end;

procedure TExampleApp.OnClearClick(widget: PGtkWidget; data: Pointer);
begin
  // Проверяем, что виджеты созданы
  if FNameEntry = nil then
  begin
    WriteLn('ОШИБКА: FNameEntry не создан');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ОШИБКА: FResultLabel не создан');
    Exit;
  end;
  
  if FCounterLabel = nil then
  begin
    WriteLn('ОШИБКА: FCounterLabel не создан');
    Exit;
  end;
  
  TPasGTK4.SetEntryText(FNameEntry, '');
  TPasGTK4.SetLabelText(FResultLabel, '');
  FClickCount := 0;
  TPasGTK4.SetLabelText(FCounterLabel, 'Кликов: 0');
  
  WriteLn('Данные очищены');
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
  
  // Создаем Adwaita приложение
  FApp := TPasGTK4.CreateAdwApplication('com.anmitalidev.pasgtk4.adwaita-example');
  if FApp = nil then
    raise Exception.Create('Не удалось создать Adwaita приложение');
  
  // Подключаем обработчик активации
  TPasGTK4.ConnectApplicationSignal(PGtkApplication(FApp), 'activate', @adw_app_activate_callback, Self);
end;

destructor TAdwaitaExampleApp.Destroy;
begin
  inherited Destroy;
end;

procedure TAdwaitaExampleApp.OnActivate(app: PAdwApplication; data: Pointer);
begin
  // Создаем Adwaita окно
  FWindow := TPasGTK4.CreateAdwWindow(FApp);
  if FWindow = nil then
  begin
    WriteLn('ОШИБКА: Не удалось создать Adwaita окно');
    Exit;
  end;
  
  // Устанавливаем размер окна
  TPasGTK4.SetWindowSize(PGtkWindow(FWindow), 500, 400);
  
  // Настраиваем интерфейс
  SetupWindow;
  
  // Показываем окно
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
  // Создаем HeaderBar (современная панель заголовка)
  FHeaderBar := TPasGTK4.CreateHeaderBar;
  
  // Создаем ToastOverlay (для уведомлений)
  FToastOverlay := TPasGTK4.CreateToastOverlay;
  if FToastOverlay = nil then
  begin
    WriteLn('ОШИБКА: Не удалось создать ToastOverlay');
    Exit;
  end;
  
  // Создаем основной контейнер
  FMainBox := TPasGTK4.CreateVerticalBox(12);
  TPasGTK4.SetWidgetMargins(PGtkWidget(FMainBox), 24, 24, 24, 24);
  
  // Устанавливаем основной контейнер в ToastOverlay
  TPasGTK4.SetToastOverlayChild(FToastOverlay, PGtkWidget(FMainBox));
  
  // Если есть HeaderBar, создаем заголовок
  if FHeaderBar <> nil then
  begin
    title_label := TPasGTK4.CreateLabel('PasGTK4 с LibAdwaita');
    TPasGTK4.SetHeaderBarTitle(FHeaderBar, PGtkWidget(title_label));
  end;
  
  // Устанавливаем содержимое окна (для AdwApplicationWindow)
  TPasGTK4.SetAdwWindowContent(FWindow, PGtkWidget(FToastOverlay));
  
  // Добавляем приветственный текст
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(TPasGTK4.CreateLabel('Добро пожаловать в современный PasGTK4!')));
  
  // Создаем разделитель
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 12, 0, 12);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(separator_box));
  
  // Счетчик кликов
  FCounterLabel := TPasGTK4.CreateLabel('Кликов: 0');
  TPasGTK4.SetLabelJustify(FCounterLabel, GTK_JUSTIFY_CENTER);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(FCounterLabel));
  
  // Кнопка для подсчета кликов
  click_button := TPasGTK4.CreateButton('Нажми меня!');
  TPasGTK4.ConnectSignal(PGtkWidget(click_button), 'clicked', @adw_button_click_callback, Self);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(click_button));
  
  // Разделитель
  separator_box := TPasGTK4.CreateHorizontalBox(0);
  TPasGTK4.SetWidgetMargins(PGtkWidget(separator_box), 0, 12, 0, 12);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(separator_box));
  
  // Поле ввода имени
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(TPasGTK4.CreateLabel('Введите ваше имя:')));
  FNameEntry := TPasGTK4.CreateEntry;
  TPasGTK4.SetEntryPlaceholder(FNameEntry, 'Ваше имя');
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(FNameEntry));
  
  // Метка для результата
  FResultLabel := TPasGTK4.CreateLabel('');
  TPasGTK4.SetLabelJustify(FResultLabel, GTK_JUSTIFY_CENTER);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(FResultLabel));
  
  // Контейнер для кнопок
  button_box := TPasGTK4.CreateHorizontalBox(8);
  TPasGTK4.SetWidgetMargins(PGtkWidget(button_box), 0, 12, 0, 0);
  TPasGTK4.AddToBox(FMainBox, PGtkWidget(button_box));
  
  // Создаем кнопки действий
  greet_button := TPasGTK4.CreateButton('Поздороваться');
  TPasGTK4.ConnectSignal(PGtkWidget(greet_button), 'clicked', @adw_greet_click_callback, Self);
  TPasGTK4.AddToBox(button_box, PGtkWidget(greet_button));
  
  clear_button := TPasGTK4.CreateButton('Очистить');
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
  // Проверяем, что виджет создан
  if FCounterLabel = nil then
  begin
    WriteLn('ОШИБКА: FCounterLabel не создан');
    Exit;
  end;
  
  Inc(FClickCount);
  TPasGTK4.SetLabelText(FCounterLabel, 'Кликов: ' + IntToStr(FClickCount));
  WriteLn('Кнопка нажата! Всего кликов: ', FClickCount);
end;

procedure TAdwaitaExampleApp.OnGreetClick(widget: PGtkWidget; data: Pointer);
var
  name: string;
  greeting: string;
begin
  // Проверяем, что виджеты созданы
  if FNameEntry = nil then
  begin
    WriteLn('ОШИБКА: FNameEntry не создан');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ОШИБКА: FResultLabel не создан');
    Exit;
  end;
  
  name := TPasGTK4.GetEntryText(FNameEntry);
  
  if Trim(name) = '' then
    greeting := 'Привет, незнакомец!'
  else
    greeting := 'Привет, ' + name + '! Добро пожаловать в современный PasGTK4!';
  
  TPasGTK4.SetLabelText(FResultLabel, greeting);
  WriteLn('Приветствие: ', greeting);
end;

procedure TAdwaitaExampleApp.OnClearClick(widget: PGtkWidget; data: Pointer);
begin
  // Проверяем, что виджеты созданы
  if FNameEntry = nil then
  begin
    WriteLn('ОШИБКА: FNameEntry не создан');
    Exit;
  end;
  
  if FResultLabel = nil then
  begin
    WriteLn('ОШИБКА: FResultLabel не создан');
    Exit;
  end;
  
  if FCounterLabel = nil then
  begin
    WriteLn('ОШИБКА: FCounterLabel не создан');
    Exit;
  end;
  
  TPasGTK4.SetEntryText(FNameEntry, '');
  TPasGTK4.SetLabelText(FResultLabel, '');
  FClickCount := 0;
  TPasGTK4.SetLabelText(FCounterLabel, 'Кликов: 0');
  WriteLn('Данные очищены');
end;

procedure TAdwaitaExampleApp.OnToastClick(widget: PGtkWidget; data: Pointer);
var
  toast: PAdwToast;
begin
  // Проверяем, что ToastOverlay создан
  if FToastOverlay = nil then
  begin
    WriteLn('ОШИБКА: FToastOverlay не создан');
    Exit;
  end;
  
  toast := TPasGTK4.CreateToast('Современное уведомление LibAdwaita!');
  if toast = nil then
  begin
    WriteLn('ОШИБКА: Не удалось создать Toast');
    Exit;
  end;
  
  TPasGTK4.ShowToast(FToastOverlay, toast);
  WriteLn('Показано Toast уведомление');
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
  
  // Проверяем параметры командной строки
  use_adwaita := (ParamCount > 0) and (ParamStr(1) = '--adwaita');
  
  if use_adwaita then
  begin
    WriteLn('Режим: LibAdwaita (современный дизайн GNOME)');
    WriteLn('');
    
    // Инициализируем PasGTK4 с LibAdwaita
    if not InitializePasGTK4WithAdwaita then
    begin
      WriteLn('ОШИБКА: Не удалось инициализировать PasGTK4 с LibAdwaita');
      WriteLn('Убедитесь, что GTK4 и LibAdwaita установлены в системе');
      Halt(1);
    end;
    
    WriteLn('PasGTK4 с LibAdwaita успешно инициализирован');
    
    try
      // Создаем и запускаем Adwaita приложение
      adw_app := TAdwaitaExampleApp.Create;
      try
        WriteLn('Запуск современного приложения...');
        result_code := adw_app.Run;
        WriteLn('Приложение завершено с кодом: ', result_code);
      finally
        adw_app.Free;
      end;
    except
      on E: Exception do
      begin
        WriteLn('ОШИБКА: ', E.Message);
        Halt(1);
      end;
    end;
  end
  else
  begin
    WriteLn('Режим: Обычный GTK4');
    WriteLn('Запустите с параметром --adwaita для современного дизайна');
    WriteLn('');
    
    // Инициализируем PasGTK4
    if not InitializePasGTK4 then
    begin
      WriteLn('ОШИБКА: Не удалось инициализировать PasGTK4');
      WriteLn('Убедитесь, что GTK4 установлен в системе');
      Halt(1);
    end;
    
    WriteLn('PasGTK4 успешно инициализирован');
    
    try
      // Создаем и запускаем обычное приложение
      app := TExampleApp.Create;
      try
        WriteLn('Запуск приложения...');
        result_code := app.Run;
        WriteLn('Приложение завершено с кодом: ', result_code);
      finally
        app.Free;
      end;
    except
      on E: Exception do
      begin
        WriteLn('ОШИБКА: ', E.Message);
        Halt(1);
      end;
    end;
  end;
  
  // Финализируем PasGTK4
  FinalizePasGTK4;
  WriteLn('PasGTK4 финализирован');
end.