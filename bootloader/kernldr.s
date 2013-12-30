;-----------------------------------------
; Second Stage Bootloader
; Enters protected mode, sets up the GDT
; and loads the kernel.
;-----------------------------------------
bits 16
org 0x0500

%define KERN_BUFF 0x1B00
%define KERN_DEST 0x10000

jmp boot2

;-----------------------------------------
; GDT Table
;-----------------------------------------

bits 16

gdt_start:

; Null Descriptor
dd 0x0
dd 0x0

; Code Descriptor
dw 0xFFFF   ; Lower 16 bits of limit
dw 0        ; Lower 16 bits of base
db 0        ; Middle 8 bits of base
db 0x9A     ; Flags: (1001): segement in memory with highest privilege.
            ; Type: execute/read code segement
db 0xCF     ; Upper segment limit, granulaity and D set
db 0        ; Upper base

; Data Descriptor
dw 0xFFFF
dw 0
db 0
db 0x92 ; R/W data segment
db 0xCF
db 0
gdt_end:
gdt_pointer:
  dw gdt_end - gdt_start - 1
  dd gdt_start

;-----------------------------------------
; A20 Enable Routines
;-----------------------------------------

; Uses the PS/2 Microcontroller to enable the
; A20 gate.
enable_a20:

push eax

call ps2_wait_input
mov al, 0xAD
out 0x64, al

call ps2_wait_input
mov al, 0xD0
out 0x64, al

call ps2_wait_output
in al, 0x60
push eax

call ps2_wait_input
mov al, 0xD1
out 0x64, al

call ps2_wait_input
pop eax
or al, 2
out 0x60, al

call ps2_wait_input
mov al, 0xAE
out 0x64, al

pop eax
ret

; Read the status register
ps2_wait_input:
  in al, 0x64
  test al, 2 ; Check input command buffer
  jnz ps2_wait_input
  ret

ps2_wait_output:
  in al, 0x64
  test al, 1
  jnz ps2_wait_input
  ret

;-----------------------------------------
; Sector reading methods
;-----------------------------------------

%define BPB_BASE 0x7C00 ; Base where FAT12 BPB is loaded
%define DIRENT_SZ 0x20
%define READ_BUFF 0x0900

; We can preserve the disk geometry and stuff from the
; bootsector. We do this because we are going to overwrite the
; bootsector with the kernel.

%define BPB_BPS (BPB_BASE + 11)
%define BPB_SPC (BPB_BASE + 13)
%define BPB_RESERVED (BPB_BASE + 14)
%define BPB_NUM_FAT (BPB_BASE + 16)
%define BPB_ROOT_ENTRIES (BPB_BASE + 17)
%define BPB_SPF (BPB_BASE + 22)
%define BPB_SPT (BPB_BASE + 24)
%define BPB_NUM_HEADS (BPB_BASE + 26)
%define BPB_DRIVE_NUM (BPB_BASE + 36)

bps:          dw 0
spc:          db 0
reserved:     dw 0
num_fat:      db 0
root_entries: dw 0
spf:          dw 0
spt:          dw 0
num_heads:    dw 0
drive_num:    dw 0
data_offset:  dw 0
kern_size:    dd 0


;-----------------------------------------
; init_floppy
; Sets up important values needed to read
; from a floppy disk
; into disk.
;-----------------------------------------

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

  ; We need to convert the LBA to a CHS address
  ; We can do so very easily! Check wikipedia!

read_sector_loop:
  
  push ax
  push cx 

  div word [spt]
  mov di, dx ; We do care about the remained; it's the S in CHS
  inc di     ; Sectors start from 1

  xor dx, dx
  div word [num_heads]
  ; ax will contain the C in the CHS. dx contains the H!

  ; We need to set up our registers for a system call now
  mov cx, ax
  xchg cl, ch  ; We must put the low 8 bits of the cylinder number in CH (they are in CL right now)
  shl cl, 6    ; cl must contain the high 2 bits of the cylinder number
  or cx, di    ; the lower 6 bits of cl will contain the sector number

  ; The drive number must be in DL and head number in DH
  mov dh, byte [drive_num]
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
  add bx, [bps]
  ; Increment LBA
  inc ax
  ; 1 less block to read
  dec cx
  jnz read_sector_loop

  popa
  ret

;-----------------------------------------
; load_file
; Reads a given sector from a given offset
; into disk.
; AX    : Filename
; ES:BX : Buffer location
; 
;-----------------------------------------

load_file:
  pusha

; Calculate the size of the root directory. Store it in cx.
  ; Formula is as follows: ((bsRootEntries * 32) + (bsBytesPerSector - 1)) / bsBytesPerSector
  ; Notice we add bsBytesPerSector-1. This is to round us up to the nearest number of sectors required to read the whole root directory.
  xor ax, ax
  xor cx, cx
  mov ax, [root_entries]
  shl ax, 5 ; Multiply by 32
  add ax, [bps]
  dec ax
  xor dx, dx
  div word [bps] ; Divide ax by bsBytesPerSector and store the quotient in ax (remainder is in dx).
  mov cx, ax

  ; Calculate the offset to the root directory.
  ; The formula is as follows: bsReservedSectors + bsNumFat * bsSectorsPerFat
  ; The first reserved sector is the bootsector, and we also need to skip over the FAT tables.
  xor ax, ax
  mov al, [num_fat]
  mul word [spf]
  add ax, [reserved]
  mov [data_offset], ax

  ; Load the root directory into memory.
  mov bx, READ_BUFF ; The root directory will be stored at 0000:7E00
  mov dx, 0
  mov es, dx
  call read_sector

  ; Look for 2nd stage bootloader in offset
  mov di, READ_BUFF - 0x20
  mov ax, [root_entries]     ; Will look through every entry
  mov si, kern_name

  search_loop:
    dec ax
    jl no_file

    mov cx, 0xB                 ; File names are 11 bytes in length.
    add di, DIRENT_SZ
    push di
    mov si, kern_name           ; Do I need to keep doing this?
    repe cmpsb
    pop di
    jne search_loop

    found_file:
      ; Offset to first cluster number 
      mov ax, [di + 0x001a]
      push ax

      ; Load FAT table into memory
      ; FAT table starts right after reserved sectors
      mov ax, [reserved]
      mov bx, READ_BUFF
      mov cx, [spf]
      mov dx, 0
      mov es, dx
      call read_sector

      ; Set up to read clusters
      pop ax ; Saved first cluster
      mov bx, KERN_BUFF
      push bx

      read_cluster:
        xor cx, cx
        mov cl, [spc]
        inc dword [kern_size]
        
        ; Hold onto the current relative cluster. It indexs the file table.
        mov bp, ax

        ; Calculate block address from cluster number.
        ; Formula: [(cluster - 2) * sectors_per_cluster + data_sector] + (dir_entries * 32 / bytes_per_sector)
        sub ax, 0x2
        mul cx
        add ax, [data_offset]
        mov bx, ax
        mov ax, [root_entries]
        shl ax, 0x5 
        xor dx, dx
        div word [bps]
        add ax, bx

        pop bx
        ; Clear es just in case
        xor dx, dx
        mov es, dx
        call read_sector

        mov ax, [bps]
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
        add ax, READ_BUFF
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
        mov ax, [kern_size]
        mul byte [spc]
        mov [kern_size], ax
        pop bx
        popa
        ret

    no_file:
      mov si, kern_name
    crash:
      lodsb ;Move one byte from SI to DI
      mov ah, 0x0E
      int 0x10
      test al, al
      jnz crash

      pop bx
      popa
      ret

;-----------------------------------------
; Entry Point
;-----------------------------------------

boot2:
; Turn off interrupts
cli

; Zero all segment registers
xor ax, ax
mov ds, ax
mov es, ax
mov fs, ax
mov gs, ax

; Set up the stack
mov ss, ax
mov ax, 0xFFFF
mov sp, ax

; Load the GDT
lgdt [gdt_pointer]

; Enable A20
call enable_a20

; Set up disk geometry parameters.
mov ax, [BPB_BPS]
mov [bps], ax
mov ax, [BPB_SPC]
mov [spc], ax
mov ax, [BPB_RESERVED]
mov [reserved], ax
mov al, [BPB_NUM_FAT]
mov [num_fat], al
mov ax, [BPB_ROOT_ENTRIES]
mov [root_entries], ax
mov ax, [BPB_SPF]
mov [spf], ax
mov ax, [BPB_SPT]
mov [spt], ax
mov ax, [BPB_NUM_HEADS]
mov [num_heads], ax
mov ax, [BPB_DRIVE_NUM]
mov [drive_num], ax

; Load the kernel from the disk.
call load_file

; Enter protected mode
cli
mov eax, cr0
or eax, 1
mov cr0, eax

jmp 0x8:pmode ; Make sure that the code selector is loaded in CS

;-----------------------------------------
; Protected Mode
;-----------------------------------------
bits 32

pmode:
mov ax, 0x10 ; 0x10 is the data selector
mov ds, ax
mov ss, ax
mov es, ax
mov fs, ax
mov gs, ax
mov esp, 0xFFFF ; Set up stack again

; Copy the kernel at the 1MB mark
xor eax, eax
mov esi, KERN_BUFF
mov edi, KERN_DEST
mov eax, [kern_size]
mul word [bps]
mov ebx, 4
div ebx
mov ecx, eax
cld
rep movsd

jmp 0x8:KERN_DEST

cli
hlt

kern_name: db "KERN       "
read_failed db "Failed reading from floppy.", 0
