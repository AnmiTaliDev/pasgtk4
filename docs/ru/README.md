# PasGTK4 - Pascal биндинги для GTK4

Современные Pascal биндинги для GTK4 с поддержкой LibAdwaita.

## Возможности

- **Поддержка GTK4**: Полные биндинги для виджетов и функциональности GTK4
- **Интеграция LibAdwaita**: Современный дизайн GNOME с компонентами Adwaita
- **Безопасность памяти**: Комплексная обработка ошибок и проверки nil
- **Кроссплатформенность**: Работает на Linux, Windows и macOS
- **Объектно-ориентированный**: Чистые Pascal классы, обертывающие функциональность GTK4
- **Условная отладка**: Отладочный вывод управляется флагами компилятора

## Быстрый старт

### Установка зависимостей

#### Ubuntu/Debian
```bash
sudo apt install libgtk-4-dev libadwaita-1-dev
```

#### Arch Linux
```bash
sudo pacman -S gtk4 libadwaita
```

**Или установить напрямую из AUR:**
```bash
yay -S pasgtk4-git
```
Пакет: https://aur.archlinux.org/packages/pasgtk4-git

### Компиляция и запуск

```bash
# Сборка с помощью Meson
meson setup builddir
meson compile -C builddir

# Запуск с LibAdwaita
./example/example_main --adwaita

# Или компиляция напрямую
fpc -Fu./src example/example_main.pas -oexample/example_main
./example/example_main
```

## Структура проекта

```
pasgtk4/
├── docs/
│   ├── ru/          # Русская документация
│   └── en/          # Английская документация
├── src/
│   ├── wrapper.pas  # Низкоуровневые биндинги GTK4
│   └── main.pas     # Высокоуровневые Pascal классы
├── example/
│   └── example_main.pas  # Базовое приложение-пример
└── README.md
```

## Архитектура

### Низкий уровень (wrapper.pas)
- Динамическая загрузка библиотек GTK4/LibAdwaita
- Прямые обертки C-функций
- Управление памятью и ошибками
- Типы данных GTK4

### Высокий уровень (main.pas)
- Объектно-ориентированные классы
- Упрощенный API для быстрой разработки
- Автоматическое управление ресурсами
- Встроенная обработка событий

## Базовое использование

```pascal
program simple_app;

uses
  SysUtils, main, wrapper;

var
  app: TExampleApp;

begin
  // Инициализация PasGTK4 с LibAdwaita
  if not InitializePasGTK4WithAdwaita then
    Halt(1);
  
  // Создание и запуск приложения
  app := TExampleApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
  
  // Очистка ресурсов
  FinalizePasGTK4;
end.
```

## Классы высокого уровня

### TGTKApplication
Базовый класс приложения с основной функциональностью:
- Управление жизненным циклом приложения
- Создание и настройка главного окна
- Обработка событий активации

### TGTKSimpleWindow
Простое окно с вертикальным или горизонтальным расположением:
- Автоматическое управление контейнерами
- Простые методы добавления виджетов
- Встроенная обработка отступов

### TGTKGridWindow
Окно с сеточным расположением для сложных интерфейсов:
- Точное позиционирование виджетов
- Поддержка многоколоночных макетов
- Гибкое управление размерами

## Основные функции

### Инициализация
- `InitializePasGTK4()`: Инициализация только GTK4
- `InitializePasGTK4WithAdwaita()`: Инициализация GTK4 + LibAdwaita
- `FinalizePasGTK4()`: Очистка ресурсов

### Создание виджетов
```pascal
// Создание виджетов
button := TPasGTK4.CreateButton('Нажми меня');
label := TPasGTK4.CreateLabel('Привет мир');
entry := TPasGTK4.CreateEntry;

// Настройка свойств
TPasGTK4.SetEntryText(entry, 'Начальный текст');
TPasGTK4.SetLabelText(label, 'Новый текст');

// Подключение сигналов
TPasGTK4.ConnectSignal(PGtkWidget(button), 'clicked', @callback, data);
```

## Отладка

Включение отладочного вывода при компиляции:
```bash
fpc -dDEBUG -Fu./src example/example_main.pas
```

## Примеры использования

Смотрите папку `example/` для полных примеров использования различных виджетов и возможностей библиотеки.

## Лицензия

Лицензировано под GNU Lesser General Public License v3.0. Подробности в файле LICENSE.md.