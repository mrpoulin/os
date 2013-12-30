# Top-level project makefile

-include kern/defs.mk
-include bootloader/defs.mk

kern-floppy : $(FLOPPY_IMG) $(K_BIN)
	mcopy -i $(FLOPPY_IMG) $(K_BIN) ::/

# Generates a 3.5" floppy disk image with the first and second
# stage bootloaders installed
# Floppy image has FAT12 with 512b sectors
$(FLOPPY_IMG) : $(BOOT1_B) $(BOOT2_B)
	dd if=/dev/zero of=$(FLOPPY_IMG) bs=512 count=2880
	mkdosfs -F 12 -S 512 $(FLOPPY_IMG)
	dd if=$(BOOT1_B) of=$(FLOPPY_IMG) bs=1 count=3 conv=notrunc
	dd if=$(BOOT1_B) of=$(FLOPPY_IMG) bs=1 count=450 skip=62 seek=62 conv=notrunc
	mcopy -i $(FLOPPY_IMG) $(BOOT2_B) ::/


.PHONY:
clean : kclean bootloader_clean
