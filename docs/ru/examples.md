# Примеры использования

## Простое приложение

```pascal
program simple_example;

uses
  SysUtils, main, wrapper;

type
  TSimpleApp = class(TGTKSimpleWindow)
  public
    constructor Create;
    procedure SetupWindow; override;
    procedure OnButtonClick(widget: PGtkWidget; data: Pointer); cdecl;
  end;

constructor TSimpleApp.Create;
begin
  inherited Create('com.example.simple');
  Title := 'Простое приложение';
  Width := 400;
  Height := 300;
end;

procedure TSimpleApp.SetupWindow;
begin
  inherited SetupWindow;
  
  AddLabel('Добро пожаловать в PasGTK4!');
  AddButton('Нажми меня', @OnButtonClick, Self);
  AddEntry('Введите текст здесь');
end;

procedure TSimpleApp.OnButtonClick(widget: PGtkWidget; data: Pointer);
begin
  WriteLn('Кнопка нажата!');
end;

var
  app: TSimpleApp;

begin
  if not InitializePasGTK4 then
    Halt(1);
    
  app := TSimpleApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  FinalizePasGTK4;
end.
```

## Приложение с сеточным расположением

```pascal
program grid_example;

uses
  SysUtils, main, wrapper;

type
  TGridApp = class(TGTKGridWindow)
  private
    FNameEntry, FEmailEntry: PGtkEntry;
    FResultLabel: PGtkLabel;
  public
    constructor Create;
    procedure SetupWindow; override;
    procedure OnSubmitClick(widget: PGtkWidget; data: Pointer); cdecl;
  end;

constructor TGridApp.Create;
begin
  inherited Create('com.example.grid');
  Title := 'Форма регистрации';
  Width := 450;
  Height := 250;
end;

procedure TGridApp.SetupWindow;
begin
  inherited SetupWindow;
  
  // Метки
  AttachLabel('Имя:', 0, 0, 1, 1);
  AttachLabel('Email:', 0, 1, 1, 1);
  
  // Поля ввода
  FNameEntry := AttachEntry(1, 0, 2, 1, 'Введите ваше имя');
  FEmailEntry := AttachEntry(1, 1, 2, 1, 'example@email.com');
  
  // Кнопка
  AttachButton('Отправить', 1, 2, 1, 1, @OnSubmitClick, Self);
  
  // Метка для результата
  FResultLabel := AttachLabel('', 0, 3, 3, 1);
end;

procedure TGridApp.OnSubmitClick(widget: PGtkWidget; data: Pointer);
var
  name, email: string;
begin
  name := TPasGTK4.GetEntryText(FNameEntry);
  email := TPasGTK4.GetEntryText(FEmailEntry);
  
  TPasGTK4.SetLabelText(FResultLabel, 
    Format('Привет, %s! Ваш email: %s', [name, email]));
end;

var
  app: TGridApp;

begin
  if not InitializePasGTK4 then
    Halt(1);
    
  app := TGridApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  FinalizePasGTK4;
end.
```

## Приложение с LibAdwaita

```pascal
program adwaita_example;

uses
  SysUtils, main, wrapper;

type
  TAdwaitaApp = class(TGTKSimpleWindow)
  private
    FToastOverlay: PAdwToastOverlay;
  public
    constructor Create;
    procedure SetupWindow; override;
    procedure ShowNotification(widget: PGtkWidget; data: Pointer); cdecl;
  end;

constructor TAdwaitaApp.Create;
begin
  inherited Create('com.example.adwaita');
  Title := 'Современное приложение';
  Width := 500;
  Height := 400;
end;

procedure TAdwaitaApp.SetupWindow;
var
  headerBar: PAdwHeaderBar;
  titleLabel: PGtkLabel;
  mainBox: PGtkBox;
begin
  // Создаем заголовочную панель
  headerBar := TPasGTK4.CreateHeaderBar;
  titleLabel := TPasGTK4.CreateLabel('PasGTK4 + Adwaita');
  TPasGTK4.SetHeaderBarTitle(headerBar, PGtkWidget(titleLabel));
  
  // Создаем toast overlay
  FToastOverlay := TPasGTK4.CreateToastOverlay;
  
  // Создаем основное содержимое
  mainBox := TPasGTK4.CreateVerticalBox(20);
  TPasGTK4.SetWidgetMargins(PGtkWidget(mainBox), 20, 20, 20, 20);
  
  // Добавляем виджеты
  TPasGTK4.AddToBox(mainBox, PGtkWidget(headerBar));
  TPasGTK4.AddToBox(mainBox, PGtkWidget(
    TPasGTK4.CreateLabel('Добро пожаловать в современный интерфейс!')));
  TPasGTK4.AddToBox(mainBox, PGtkWidget(
    TPasGTK4.CreateButton('Показать уведомление')));
  
  // Подключаем сигнал кнопки
  TPasGTK4.ConnectSignal(
    PGtkWidget(TPasGTK4.CreateButton('Уведомление')), 
    'clicked', @ShowNotification, Self);
  
  // Устанавливаем содержимое overlay
  TPasGTK4.SetToastOverlayChild(FToastOverlay, PGtkWidget(mainBox));
  
  // Устанавливаем содержимое окна
  TPasGTK4.SetWindowChild(GetWindow, PGtkWidget(FToastOverlay));
end;

procedure TAdwaitaApp.ShowNotification(widget: PGtkWidget; data: Pointer);
var
  toast: PAdwToast;
begin
  toast := TPasGTK4.CreateToast('Это современное уведомление!');
  TPasGTK4.ShowToast(FToastOverlay, toast);
end;

var
  app: TAdwaitaApp;

begin
  if not InitializePasGTK4WithAdwaita then
    Halt(1);
    
  app := TAdwaitaApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  FinalizePasGTK4;
end.
```

## Калькулятор

```pascal
program calculator;

uses
  SysUtils, main, wrapper;

type
  TCalculator = class(TGTKGridWindow)
  private
    FDisplay: PGtkEntry;
    FCurrentValue: Double;
    FOperation: Char;
    FWaitingForOperand: Boolean;
  public
    constructor Create;
    procedure SetupWindow; override;
    procedure OnNumberClick(widget: PGtkWidget; data: Pointer); cdecl;
    procedure OnOperationClick(widget: PGtkWidget; data: Pointer); cdecl;
    procedure OnEqualsClick(widget: PGtkWidget; data: Pointer); cdecl;
    procedure OnClearClick(widget: PGtkWidget; data: Pointer); cdecl;
  end;

constructor TCalculator.Create;
begin
  inherited Create('com.example.calculator');
  Title := 'Калькулятор PasGTK4';
  Width := 300;
  Height := 400;
  FCurrentValue := 0;
  FOperation := #0;
  FWaitingForOperand := False;
end;

procedure TCalculator.SetupWindow;
var
  i, j, num: Integer;
  buttons: array[0..3, 0..3] of string = (
    ('7', '8', '9', '/'),
    ('4', '5', '6', '*'),
    ('1', '2', '3', '-'),
    ('C', '0', '=', '+')
  );
  button: PGtkButton;
begin
  inherited SetupWindow;
  
  // Дисплей
  FDisplay := AttachEntry(0, 0, 4, 1, '0');
  TPasGTK4.SetEntryText(FDisplay, '0');
  
  // Кнопки
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      button := AttachButton(buttons[i, j], j, i + 1, 1, 1, nil, nil);
      
      case buttons[i, j] of
        '0'..'9':
          TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @OnNumberClick, 
            Pointer(PtrInt(StrToInt(buttons[i, j]))));
        '+', '-', '*', '/':
          TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @OnOperationClick,
            Pointer(PtrInt(Ord(buttons[i, j][1]))));
        '=':
          TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @OnEqualsClick, Self);
        'C':
          TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @OnClearClick, Self);
      end;
    end;
end;

procedure TCalculator.OnNumberClick(widget: PGtkWidget; data: Pointer);
var
  digit: Integer;
  currentText: string;
begin
  digit := PtrInt(data);
  currentText := TPasGTK4.GetEntryText(FDisplay);
  
  if FWaitingForOperand or (currentText = '0') then
  begin
    TPasGTK4.SetEntryText(FDisplay, IntToStr(digit));
    FWaitingForOperand := False;
  end
  else
    TPasGTK4.SetEntryText(FDisplay, currentText + IntToStr(digit));
end;

procedure TCalculator.OnOperationClick(widget: PGtkWidget; data: Pointer);
begin
  FCurrentValue := StrToFloatDef(TPasGTK4.GetEntryText(FDisplay), 0);
  FOperation := Chr(PtrInt(data));
  FWaitingForOperand := True;
end;

procedure TCalculator.OnEqualsClick(widget: PGtkWidget; data: Pointer);
var
  operand, result: Double;
begin
  operand := StrToFloatDef(TPasGTK4.GetEntryText(FDisplay), 0);
  
  case FOperation of
    '+': result := FCurrentValue + operand;
    '-': result := FCurrentValue - operand;
    '*': result := FCurrentValue * operand;
    '/': if operand <> 0 then result := FCurrentValue / operand else result := 0;
    else result := operand;
  end;
  
  TPasGTK4.SetEntryText(FDisplay, FloatToStr(result));
  FOperation := #0;
  FWaitingForOperand := True;
end;

procedure TCalculator.OnClearClick(widget: PGtkWidget; data: Pointer);
begin
  TPasGTK4.SetEntryText(FDisplay, '0');
  FCurrentValue := 0;
  FOperation := #0;
  FWaitingForOperand := False;
end;

var
  calc: TCalculator;

begin
  if not InitializePasGTK4 then
    Halt(1);
    
  calc := TCalculator.Create;
  try
    calc.Run;
  finally
    calc.Free;
  end;
  
  FinalizePasGTK4;
end.
```

## Компиляция примеров

```bash
# Простая компиляция
fpc -Fu./src example_name.pas -oexample_name

# С отладкой
fpc -dDEBUG -Fu./src example_name.pas -oexample_name

# С оптимизацией
fpc -O3 -Fu./src example_name.pas -oexample_name
```