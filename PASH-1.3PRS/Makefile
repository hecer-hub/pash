SRCDIR = modul
OBJDIR = oppu

FPCFLAGS_BETA = -Fu$(SRCDIR) -FU$(OBJDIR) -g
FPCFLAGS_RLS = -Fu$(SRCDIR) -FU$(OBJDIR) -O3 -Os -XX

all: rls

beta: main.pas
	fpc $(FPCFLAGS_BETA) main.pas -opashb

rls: main.pas
	fpc $(FPCFLAGS_RLS) main.pas -opashr

clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.ppu pashb pashr
