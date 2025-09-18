#!/bin/bash
detect_package_manager() {
    if command -v pacman &> /dev/null; then
        echo "pacman"
    elif command -v apt-get &> /dev/null; then
        echo "apt"
    elif command -v dnf &> /dev/null; then
        echo "dnf"
    elif command -v yum &> /dev/null; then
        echo "yum"
    else
        echo "неизвестный"
    fi
}

PKG_MANAGER=$(detect_package_manager)

if [[ "$PKG_MANAGER" == "неизвестный" ]]; then
    echo "Ошибка: Не удалось определить пакетный менеджер. Пожалуйста, установите fpc, wget и curl вручную."
    exit 1
fi

echo "Проверка и установка fpc..."
FPC_VERSION=$(fpc -iV 2>/dev/null)
if [[ "$FPC_VERSION" == "3.2.2" ]]; then
    echo "FPC версии 3.2.2 уже установлен. Продолжаем."
else
    echo "FPC не установлен или версия не 3.2.2. Попытка установки..."
    case "$PKG_MANAGER" in
        pacman)
            sudo pacman -Syu --noconfirm fpc=3.2.2
            ;;
        apt)
            sudo apt-get update
            sudo apt-get install -y fpc
            ;;
        dnf|yum)
            sudo "$PKG_MANAGER" install -y fpc
            ;;
    esac
fi

if ! command -v fpc &> /dev/null; then
    echo "Ошибка: Не удалось установить FPC. Пожалуйста, установите его вручную и запустите скрипт снова."
    exit 1
fi

case "$PKG_MANAGER" in
    pacman)
        sudo pacman -S --noconfirm wget curl
        ;;
    apt)
        sudo apt-get install -y wget curl
        ;;
    dnf|yum)
        sudo "$PKG_MANAGER" install -y wget curl
        ;;
esac

echo "Компиляция основного проекта..."
if [ -f "Makefile" ]; then
    make rls
else
    echo "Ошибка: Файл Makefile не найден в текущей директории. Убедитесь, что вы запускаете скрипт из корневой папки проекта."
    exit 1
fi



rm -rf oppu


echo "Компиляция плагина prl.pas..."
cd binp/sncp || { echo "Ошибка: Директория binp/sncp не найдена. Убедитесь, что она существует."; exit 1; }
fpc prl.pas -O3 -Os -XX -oprl
mv prl ../
rm -f *.o *.ppu 2>/dev/null
cd ../..

echo "Компиляция завершена успешно. Для запуска пропишите ./pashr"
