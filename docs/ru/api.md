# Справочник API

## TPasGTK4 - Основной класс биндингов

### Инициализация и завершение

#### `Initialize(): Boolean`
Инициализирует GTK4 библиотеку.
- **Возвращает**: `True` при успехе, `False` при ошибке

#### `InitializeAdwaita(): Boolean`
Инициализирует LibAdwaita (требует предварительной инициализации GTK4).
- **Возвращает**: `True` при успехе, `False` при ошибке

#### `Finalize()`
Освобождает ресурсы и выгружает библиотеки.

#### `IsInitialized(): Boolean`
Проверяет, инициализирован ли GTK4.

#### `IsAdwaitaInitialized(): Boolean`
Проверяет, инициализирован ли LibAdwaita.

### Приложения

#### `CreateApplication(app_id: string): PGtkApplication`
Создает GTK4 приложение.
- **app_id**: Идентификатор приложения (например, 'com.example.myapp')
- **Возвращает**: Указатель на приложение или `nil` при ошибке

#### `RunApplication(app: PGtkApplication): Integer`
Запускает главный цикл приложения.
- **Возвращает**: Код завершения приложения

#### `ConnectApplicationSignal(app, signal, callback, data)`
Подключает обработчик сигнала к приложению.

### Окна

#### `CreateWindow(app: PGtkApplication): PGtkWindow`
Создает окно приложения.

#### `SetWindowTitle(window: PGtkWindow; title: string)`
Устанавливает заголовок окна.

#### `SetWindowSize(window: PGtkWindow; width, height: Integer)`
Устанавливает размер окна по умолчанию.

#### `ShowWindow(window: PGtkWindow)`
Отображает окно.

#### `SetWindowChild(window: PGtkWindow; child: PGtkWidget)`
Устанавливает дочерний виджет окна.

### Виджеты

#### `CreateButton(text: string): PGtkButton`
Создает кнопку с текстом.

#### `CreateLabel(text: string): PGtkLabel`
Создает метку с текстом.

#### `SetLabelText(label: PGtkLabel; text: string)`
Изменяет текст метки.

#### `SetLabelJustify(label: PGtkLabel; justify: GtkJustification)`
Устанавливает выравнивание текста метки.

#### `CreateEntry(): PGtkEntry`
Создает поле ввода текста.

#### `SetEntryText(entry: PGtkEntry; text: string)`
Устанавливает текст в поле ввода.

#### `GetEntryText(entry: PGtkEntry): string`
Получает текст из поля ввода.

#### `SetEntryPlaceholder(entry: PGtkEntry; text: string)`
Устанавливает placeholder текст.

### Контейнеры

#### `CreateBox(orientation: GtkOrientation; spacing: Integer): PGtkBox`
Создает контейнер-коробку.
- **orientation**: `GTK_ORIENTATION_HORIZONTAL` или `GTK_ORIENTATION_VERTICAL`
- **spacing**: Расстояние между элементами

#### `CreateVerticalBox(spacing: Integer): PGtkBox`
Создает вертикальную коробку.

#### `CreateHorizontalBox(spacing: Integer): PGtkBox`
Создает горизонтальную коробку.

#### `AddToBox(box: PGtkBox; widget: PGtkWidget)`
Добавляет виджет в конец коробки.

#### `PrependToBox(box: PGtkBox; widget: PGtkWidget)`
Добавляет виджет в начало коробки.

#### `CreateGrid(): PGtkGrid`
Создает сеточный контейнер.

#### `AttachToGrid(grid, widget, left, top, width, height)`
Размещает виджет в сетке с заданными координатами и размером.

### Выравнивание и отступы

#### `SetWidgetAlign(widget: PGtkWidget; halign, valign: GtkAlign)`
Устанавливает горизонтальное и вертикальное выравнивание виджета.

Константы выравнивания:
- `GTK_ALIGN_FILL` - заполнить доступное пространство
- `GTK_ALIGN_START` - выровнять к началу
- `GTK_ALIGN_END` - выровнять к концу
- `GTK_ALIGN_CENTER` - центрировать
- `GTK_ALIGN_BASELINE` - выровнять по базовой линии

#### `SetWidgetMargins(widget, top, bottom, start, end_margin)`
Устанавливает отступы виджета.

### Сигналы

#### `ConnectSignal(widget, signal, callback, data)`
Подключает обработчик сигнала к виджету.

Типичные сигналы:
- `'clicked'` - для кнопок
- `'changed'` - для полей ввода
- `'activate'` - для активации

### LibAdwaita методы

#### `CreateAdwApplication(app_id: string): PAdwApplication`
Создает Adwaita приложение.

#### `CreateAdwWindow(app: PAdwApplication): PAdwApplicationWindow`
Создает Adwaita окно.

#### `SetAdwWindowContent(window, content)`
Устанавливает содержимое Adwaita окна.

#### `CreateHeaderBar(): PAdwHeaderBar`
Создает заголовочную панель.

#### `CreateToastOverlay(): PAdwToastOverlay`
Создает overlay для toast уведомлений.

#### `CreateToast(title: string): PAdwToast`
Создает toast уведомление.

#### `ShowToast(overlay: PAdwToastOverlay; toast: PAdwToast)`
Показывает toast уведомление.

## Высокоуровневые классы

### TGTKApplication

Базовый класс для GTK4 приложений.

#### Свойства
- `Title: string` - заголовок окна
- `Width, Height: Integer` - размеры окна
- `OnActivate: TGtkApplicationCallback` - обработчик активации

#### Методы
- `constructor Create(app_id: string)` - создание приложения
- `Run(): Integer` - запуск приложения
- `SetupWindow()` - виртуальный метод настройки окна
- `GetWindow(): PGtkWindow` - получение указателя на окно
- `GetApplication(): PGtkApplication` - получение указателя на приложение

### TGTKSimpleWindow

Простое окно с линейным расположением виджетов.

#### Конструктор
```pascal
Create(app_id: string; vertical: Boolean = True; spacing: Integer = 10)
```

#### Свойства
- `MainBox: PGtkBox` - основной контейнер
- `IsVertical: Boolean` - вертикальное расположение
- `Spacing: Integer` - расстояние между виджетами

#### Методы
- `AddWidget(widget: PGtkWidget)` - добавить виджет
- `AddButton(text, callback, data): PGtkButton` - добавить кнопку
- `AddLabel(text): PGtkLabel` - добавить метку
- `AddEntry(placeholder): PGtkEntry` - добавить поле ввода

### TGTKGridWindow

Окно с сеточным расположением виджетов.

#### Свойства
- `MainGrid: PGtkGrid` - основная сетка

#### Методы
- `AttachWidget(widget, left, top, w, h)` - разместить виджет
- `AttachButton(text, left, top, w, h, callback, data): PGtkButton`
- `AttachLabel(text, left, top, w, h): PGtkLabel`
- `AttachEntry(left, top, w, h, placeholder): PGtkEntry`

## Утилиты

### `InitializePasGTK4(): Boolean`
Инициализирует только GTK4.

### `InitializePasGTK4WithAdwaita(): Boolean`
Инициализирует GTK4 и LibAdwaita.

### `FinalizePasGTK4()`
Очищает все ресурсы.

### `GetPasGTK4Version(): string`
Возвращает версию библиотеки.