# Common makefile definitions

NASM    := nasm
CXX     := i686-elf-gcc
GAS     := i686-elf-as
CFLAGS  := -Wall -Wextra -ffreestanding -std=gnu99
LDFLAGS := -ffreestanding -O2 -nostdlib
