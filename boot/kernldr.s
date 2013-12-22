;-----------------------------------------
; Second Stage Bootloader
; Enters protected mode and loads the 
; kernel.
;-----------------------------------------
bits 16
org 0x0500

cli
hlt
