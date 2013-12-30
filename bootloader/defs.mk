# Makefile definitons for the bootloader.
# All paths relative to the top level directory.

BOOTLOADER_SRC      := bootloader
BOOTLOADER_BUILDDIR := bootloader/build

BOOT1_S := $(BOOTLOADER_SRC)/bootsector.s
BOOT2_S := $(BOOTLOADER_SRC)/kernldr.s

BOOT1_B := $(BOOTLOADER_BUILDDIR)/bootsector
BOOT2_B := $(BOOTLOADER_BUILDDIR)/KERNLDR

FLOPPY_IMG := emulator/media/floppy.img

$(BOOT1_B) : $(BOOT1_S) | $(BOOTLOADER_BUILDDIR)
	nasm $< -o $@

$(BOOT2_B) : $(BOOT2_S) | $(BOOTLOADER_BUILDDIR)
	nasm $< -o $@

$(BOOTLOADER_BUILDDIR):
	mkdir -p $(BOOTLOADER_BUILDDIR)

.PHONY:
bootloader_clean:
	rm -rf $(BOOTLOADER_BUILDDIR)
	rm -f  $(FLOPPY_IMG)
