# Makefile definitions for the kernel

-include common.mk

K_SRC      := kern
K_SRCDIRS  := $(addprefix $(K_SRC)/, boot menu)
K_CPPSRC   := $(notdir $(foreach sdir, $(K_SRCDIRS), $(wildcard $(sdir)/*.cpp)))
K_ASMSRC   := $(notdir $(foreach sdir, $(K_SRCDIRS), $(wildcard $(sdir)/*.s)))

K_BUILDDIR := kern/build

K_CPPOBJ   := $(patsubst %.cpp, $(K_BUILDDIR)/%.o, $(K_CPPSRC))
K_ASMOBJ   := $(patsubst %.s, $(K_BUILDDIR)/%.o, $(K_ASMSRC))
K_BIN      := $(K_BUILDDIR)/KERN

LDSCRIPT = $(K_SRC)/conf/linker.ld

CFLAGS += -MMD -MP

vpath %.s $(K_SRCDIRS)
vpath %.cpp $(K_SRCDIRS)

-include $(CPPOBJ:.cpp=.d)

# Build ASM files
$(K_ASMOBJ) : $(K_BUILDDIR)/%.o : %.s | $(K_BUILDDIR)
	$(GAS) $< -o $@

$(K_CPPOBJ): $(K_BUILDDIR)/%.o : %.cpp | $(K_BUILDDIR)
	$(CXX) -c $(CFLAGS) $< -o $@

$(K_BUILDDIR):
	mkdir -p $@

$(K_BIN) : $(K_ASMOBJ) $(K_CPPOBJ)
	$(CXX) -T $(LDSCRIPT) -o $(K_BIN) $(LDFLAGS) $^ -lgcc

.PHONY:
kclean:
	rm -rf $(K_BUILDDIR)

