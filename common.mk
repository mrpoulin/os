# Common makefile definitions

NASM    := nasm
CXX     := i686-elf-g++
GAS     := i686-elf-as
CFLAGS  := -Wall -Wextra -ffreestanding -O2 -fno-exceptions -fno-rtti
LDFLAGS := -ffreestanding -O2 -nostdlib
