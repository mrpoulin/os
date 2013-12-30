
ENTRY(_start)

SECTIONS
{

  . = 1M;
  
  .text BLOCK(4K) :
  {
    *(.text)
  }

  .rodata BLOCK(4K) :
  {
    *(.rodata)
  }

  .data BLOCK(4K) :
  {
    *(.data)
  }

  .bss BLOCK(4K) :
  {
    *(.COMMON)
    *(.bss)
    *(.bootstrap_stack)
  }

}
