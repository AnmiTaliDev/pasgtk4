project('pasgtk4', 'c',
  version : '1.0.0',
  license : 'Apache-2.0')

# Находим Pascal компилятор
fpc = find_program('fpc', required: true)

# Настройки компилятора Pascal
pascal_args = [
  '-Mobjfpc',    # Object Pascal mode
  '-Sh',         # Use AnsiStrings
  '-O2',         # Optimization level 2
]

# Создаем исполняемый файл примера
example_target = custom_target('pasgtk4_example',
  output: 'pasgtk4_example',
  command: [
    fpc,
    pascal_args,
    '-o' + join_paths(meson.current_build_dir(), 'pasgtk4_example'), # Указываем полный путь
    '-Fi' + join_paths(meson.current_source_dir(), 'src'),
    '-Fu' + join_paths(meson.current_source_dir(), 'src'),
    join_paths(meson.current_source_dir(), 'example', 'example_main.pas')
  ],
  build_by_default: true
)

# Информация о сборке
message('Building PasGTK4 version @0@'.format(meson.project_version()))
message('Pascal compiler: @0@'.format(fpc.full_path()))