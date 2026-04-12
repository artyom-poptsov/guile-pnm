## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-PNM.
##
## Guile-PNM is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-PNM is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-PNM.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
  p1.scm \
  p1-context.scm \
  p2.scm \
  p2-context.scm \
  p3.scm \
  p3-context.scm \
  pnm.scm \
  pnm-context.scm

BUILT_SOURCES = \
  context.scm

INSTALL += \
  fsm-context.scm

EXTRA_DIST += \
  p1.scm \
  p2.scm \
  p3.scm \
  pnm.scm \
  fsm.puml

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/pnm/fsm/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/pnm/fsm/

GUILE_SMC_CONTEXT_ARGS = \
  --log-driver file \
  --log-opt "file=fsm-context.log" \
  --type functional/char \
  -m "(pnm fsm context)"

GUILE_SMC_P1_ARGS = \
  --log-driver file \
  --log-opt "file=p1.log" \
  -L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
  --fsm-name p1-fsm \
  --fsm-module "(pnm fsm p1)" \
  -U "((pnm fsm context) (pnm fsm p1-context))" \
  --validate

GUILE_SMC_P2_ARGS = \
  --log-driver file \
  --log-opt "file=p2.log" \
  -L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
  --fsm-name p2-fsm \
  --fsm-module "(pnm fsm p2)" \
  -U "((pnm fsm context) (pnm fsm p2-context))" \
  --validate

GUILE_SMC_P3_ARGS = \
  --log-driver file \
  --log-opt "file=p3.log" \
  -L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
  --fsm-name p3-fsm \
  --fsm-module "(pnm fsm p3)" \
  -U "((pnm fsm context) (pnm fsm p3-context))" \
  --validate

GUILE_SMC_PNM_ARGS = \
  --log-driver file \
  --log-opt "file=pnm.log" \
  -L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
  --fsm-name pnm-fsm \
  --fsm-module "(pnm fsm pnm)" \
  -U "((pnm fsm context) (pnm fsm pnm-context))" \
  --validate

GUILE_SMC_ENV = GUILE_AUTO_COMPILE=0

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
  --standalone
GUILE_SMC_P1_ARGS += \
  --target=guile-standalone
GUILE_SMC_P2_ARGS += \
  --target=guile-standalone
GUILE_SMC_P3_ARGS += \
  --target=guile-standalone
GUILE_SMC_PNM_ARGS += \
  --target=guile-standalone
endif

context.scm:
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

p1.scm: p1-context.scm p1.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_P1_ARGS) p1.puml > $@

p2.scm: p2-context.scm p2.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_P2_ARGS) p2.puml > $@

p3.scm: p3-context.scm p3.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_P3_ARGS) p3.puml > $@

pnm.scm: pnm-context.scm pnm.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_PNM_ARGS) pnm.puml > $@

# XXX: Automake does not remove those files for some reason.
uninstall-hook:
	rm -f $(DESTDIR)$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/pnm/fsm/context.scm \
		$(DESTDIR)$(libdir)/guile/3.0/site-ccache/pnm/fsm/context.scm

CLEANFILES += \
  p1.scm	\
  p1.log	\
  p2.scm	\
  p2.log	\
  p3.scm	\
  p3.log	\
  pnm.scm	\
  pnm.log	\
  context.scm	\
  context.log

### Makefile.am ends here.
