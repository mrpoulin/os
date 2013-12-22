;-----------------------------------------
; Simple FAT bootloader
;-----------------------------------------

; We have bytes 0x7E00 - 0x7FFFF free for use!
; We also have byes 0x500 - 0x7BFF free for use
%define FAT_TABLE_BUFF 0x7e00
%define ROOT_DIR_BUFF 0x7e00 ; Reuse after done with root directory

; Load the second bootloader at 0x0500
%define KERNLDR_BUFF 0x0500

; Size of FAT directory entry
%define DIRENT_SZ 0x20

; 16 bit real-mode
bits 16

; Since the BIOS loads this code at address 0x7c00,
; we must ensure that all instructions are relative that
; address.
; We jump past the bpb block since it is not executable code.
org 0x7c00
start:
  jmp loader ;short jump

;-----------------------------------------
; BPB
; Defines the fields found on a drive formatted as
; FAT12/16/32
;-----------------------------------------

times 3 - ($-$$) db 0 

bpb:
bsOemName:           db "MarcOS ",0 ; Up to 8 bytes in length.
bsBytesPerSector:    dw 0 ; Set by the disk formatting utility
bsSectorsPerCluster: db 0
bsReservedSectors:   dw 0
bsNumFat:            db 0
bsRootEntries:       dw 0
bsNumSectors:        dw 0
bsMediaType:         db 0
bsSectorsPerFat:     dw 0
bsSectorsPerTrack:   dw 0 ; Must be obtained from BIOS
bsNumHeads:          dw 0 ; Must be obtained from BIOS
bsHiddenSectors:     dd 0
bsLargeSectors:      dd 0

;--------------36 BIT MARK----------------

;-----------------------------------------
; EBPB
; Defines the extended fields 
; found on a drive formatted as FAT12/16
;-----------------------------------------

ebsDriveNum: db 0
dataSector:  db 0
ebsSig:      db 0
data_block:  dw 0
ebsVolId:    dw 0
ebsVolLabel: db "MOS FLOPPY "
ebsSysId:    db "FAT12   "

;--------------62 BIT MARK----------------

;-----------------------------------------
; read_sector
; Reads a given sector from a given offset
; into disk.
; AX    : Logical Block Address  
;         of first sector
; CX    : Sectors to read
; ES:BX : Buffer location
; 
;-----------------------------------------
read_sector:
  pusha

  ; Hidden sectors?

  ; We need to convert the LBA to a CHS address
  ; We can do so very easily! Check wikipedia!

read_sector_loop:
  
  push ax
  push cx 

  div word [bsSectorsPerTrack]
  mov di, dx ; We do care about the remained; it's the S in CHS
  inc di     ; Sectors start from 1

  xor dx, dx
  div word [bsNumHeads]
  ; ax will contain the C in the CHS. dx contains the H!

  ; We need to set up our registers for a system call now
  mov cx, ax
  xchg cl, ch  ; We must put the low 8 bits of the cylinder number in CH (they are in CL right now)
  shl cl, 6    ; cl must contain the high 2 bits of the cylinder number
  or cx, di    ; the lower 6 bits of cl will contain the sector number

  ; The drive number must be in DL and head number in DH
  mov dh, byte [ebsDriveNum]
  xchg dh, dl

  ;ES:BX already set up!

  mov di, 3

retry_read:
  mov ax, 0x0201 ; We only ready one sector at a time
  int 0x13
  jnc continue_read

  ; Read not successful? Reset disk.
  xor ax, ax
  int 0x13

  dec di
  jge retry_read

  ; Read failed 3 times. Crash.
  mov si, read_failed
  jmp crash
  cli
  hlt

continue_read:
  ; Restore the stack
  pop cx
  pop ax

  ; Increment destination buffer
  add bx, [bsBytesPerSector]
  ; Increment LBA
  inc ax
  ; 1 less block to read
  dec cx
  jnz read_sector_loop

  popa
  ret

;-----------------------------------------
; loader
; First bootloader. Sets up segments, loads 
; second bootloader from floppy.
;-----------------------------------------

loader:

  ; Zero segement registers
  xor ax, ax
  mov ds, ax
  mov es, ax

  ; Set up program stack. Stack is address as SS:SP
  mov ss, ax
  mov sp, 0xFFFF ; Not sure about this.

  ; Load correct values for FAT header from BIOS
  mov dl, [ebsDriveNum]
  mov ah, 0x8
  int 0x13

  ; Get sectors per track.
  ; CL [0..5] contains the maximum sector number
  and cx, 0x003F
  mov [bsSectorsPerTrack], cx

  ; Get number of heads
  mov cl, dh
  inc cl
  mov [bsNumHeads], cl

  ; Calculate the size of the root directory. Store it in cx.
  ; Formula is as follows: ((bsRootEntries * 32) + (bsBytesPerSector - 1)) / bsBytesPerSector
  ; Notice we add bsBytesPerSector-1. This is to round us up to the nearest number of sectors required to read the whole root directory.
  xor ax, ax
  xor cx, cx
  mov ax, [bsRootEntries]
  shl ax, 5 ; Multiply by 32
  add ax, [bsBytesPerSector]
  dec ax
  xor dx, dx
  div word [bsBytesPerSector] ; Divide ax by bsBytesPerSector and store the quotient in ax (remainder is in dx).
  mov cx, ax

  ; Calculate the offset to the root directory.
  ; The formula is as follows: bsReservedSectors + bsNumFat * bsSectorsPerFat
  ; The first reserved sector is the bootsector, and we also need to skip over the FAT tables.
  xor ax, ax
  mov al, [bsNumFat]
  mul word [bsSectorsPerFat]
  add ax, [bsReservedSectors]
  mov [data_block], ax

  ; Load the root directory into memory.
  mov bx, ROOT_DIR_BUFF ; The root directory will be stored at 0000:7E00
  push cs
  pop es
  call read_sector

  ; Look for 2nd stage bootloader in offset
  mov di, ROOT_DIR_BUFF - 0x20
  mov ax, [bsRootEntries]     ; Will look through every entry
  mov si, file_name           

  search_loop:
    dec ax
    jl no_file

    mov cx, 0xB                 ; File names are 11 bytes in length.
    add di, DIRENT_SZ
    mov si, file_name           ; Do I need to keep doing this?
    repe cmpsb
    jne search_loop

  found_file:
    ; di is advanced by the length of the filename string during the cmpsb.
    mov ax, [di + 0x001a - 0xb]
    push ax

    ; Load FAT table into memory
    ; FAT table starts right after reserved sectors
    mov ax, [bsReservedSectors]
    mov bx, FAT_TABLE_BUFF
    mov cx, [bsSectorsPerFat]
    push cs
    pop es
    call read_sector

    ; Set up to read clusters
    pop ax ; Saved first cluster
    mov bx, KERNLDR_BUFF
    push bx

    read_cluster:
      xor cx, cx
      mov cl, [bsSectorsPerCluster]

      
      ; Hold onto the current relative cluster. It indexs the file table.
      mov bp, ax

      ; Calculate block address from cluster number.
      ; Formula: [(cluster - 2) * sectors_per_cluster + data_sector] + (dir_entries * 32 / bytes_per_sector)
      sub ax, 0x2
      mul cx
      add ax, [data_block]
      mov bx, ax
      mov ax, [bsRootEntries]
      shl ax, 0x5 
      xor dx, dx
      div word [bsBytesPerSector]
      add ax, bx

      pop bx
      ; Clear es just in case
      push cs
      pop es
      call read_sector

      mov ax, [bsBytesPerSector]
      mul cx
      add bx, ax
      ; bx will hold the next read location.
      push bx

      ; Calculate the next cluster location. Last cluster in dx
      mov ax, bp
      ; Multiply current cluster by 1.5 and add offset of FAT table.
      ; This is to get the location in memory where the next cluster is stored.
      mov cx, 3
      mul cx
      shr ax, 0x1
      pushf ; Parity set in CF
      add ax, FAT_TABLE_BUFF
      mov bx, ax 
      mov ax, [bx] ; Overwrite AX with the next cluster.
      popf
      jnc even_cluster

      shr ax, 4 ; Weird stuff because FAT12 stores 12 bit clusters which need to be extracted from 16bit values.
      jmp cont

    even_cluster:
      and ax, 0x0FFF

    cont:
      cmp ax, 0x0FF8
      jb read_cluster ; The cluster value is >= 0xFF8 if we are at the end of the chain

    done:
      jmp 0x0000:KERNLDR_BUFF

  no_file:
    mov si, file_name
  crash:
    lodsb ;Move one byte from SI to DI
    mov ah, 0x0E
    int 0x10
    test al, al
    jnz crash
    ;End of execution
    cli 
    hlt

;--------------Strings--------------

file_name db "KERNLDR    " ; This falls through to the next string...
read_failed db "Failed reading from floppy.", 0

; Fill remaining bytes with 0
times 510 - ($-$$) db 0 

; Boot signature
dw 0xAA55

;--------------512 BIT MARK. END OF BOOTSECTOR--------------
