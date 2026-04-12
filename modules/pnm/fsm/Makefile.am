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
	pbm.scm	\
	pbm-context.scm \
	pgm.scm	\
	pgm-context.scm

BUILT_SOURCES = \
	context.scm

INSTALL += \
	fsm-context.scm

EXTRA_DIST += \
	pbm.scm	\
	pgm.scm \
	fsm.puml

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/pnm/fsm/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/pnm/fsm/

GUILE_SMC_CONTEXT_ARGS = \
	--log-driver file \
	--log-opt "file=fsm-context.log" \
	--type functional/char \
	-m "(pnm fsm context)"

GUILE_SMC_PBM_ARGS = \
	--log-driver file \
	--log-opt "file=pbm.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name pbm-fsm \
	--fsm-module "(pnm fsm pbm)" \
	-U "((pnm fsm context) (pnm fsm pbm-context))" \
	--validate

GUILE_SMC_PGM_ARGS = \
	--log-driver file \
	--log-opt "file=pbm.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name pgm-fsm \
	--fsm-module "(pnm fsm pgm)" \
	-U "((pnm fsm context) (pnm fsm pgm-context))" \
	--validate

GUILE_SMC_ENV = GUILE_AUTO_COMPILE=0

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
	--standalone
GUILE_SMC_PBM_ARGS += \
	--target=guile-standalone
GUILE_SMC_PGM_ARGS += \
	--target=guile-standalone
endif

context.scm:
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

pbm.scm: pbm-context.scm pbm.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_PBM_ARGS) pbm.puml > $@

pgm.scm: pgm-context.scm pgm.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_PGM_ARGS) pgm.puml > $@


# XXX: Automake does not remove those files for some reason.
uninstall-hook:
	rm -f $(DESTDIR)$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/pnm/fsm/context.scm \
		$(DESTDIR)$(libdir)/guile/3.0/site-ccache/pnm/fsm/context.scm

CLEANFILES += \
	pbm.scm	\
	pbm.log	\
	pgm.scm	\
	pgm.log	\
	context.scm	\
	context.log

### Makefile.am ends here.
