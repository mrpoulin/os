ENTRY(_start)

SECTIONS 
{

  . = 0x100000;

  .text :
  {
    *(.text)
    *(.rodata)
  }

  .data ALIGN(4K) :
  {
    *(.data)
  }

  .bss ALIGN(4K) :
  {
    *(COMMON)
    *(.bss)
    *(.bootstrap_stack)
  }
}
