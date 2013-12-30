
.section .bootstrap_stack
stack_bottom:
.skip 16384
stack_top:

.section .text
.global _start
.type _start, @function
_start:
  mov $stack_top, %esp

  call kmain

  cli
  hlt

.Lhang:
  jmp .Lhang
