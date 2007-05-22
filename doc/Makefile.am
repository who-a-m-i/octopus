# $Id$

SUBDIRS = html

man_MANS = octopus.1 oct-atomic_occupations.1 oct-broad.1 oct-center-geom.1 oct-cross-section.1 \
        oct-display_partitions.1 oct-harmonic-spectrum.1 oct-ph.1 oct-rsf.1 \
	oct-run_periodic_table.1 oct-run_regression_test.pl.1 oct-run_testsuite.1 oct-xyz-anim.1

docdir = $(pkgdatadir)/doc
dist_doc_DATA = octopus.pdf

EXTRA_DIST = *.pod img/*.eps img/*.jpg $(man_MANS)

CLEANFILES = *~ *.bak $(man_MANS)

.pod.1:
	pod2man -c "octopus documentation" -s 1 $< > $@

octopus.pdf:
	@wget "http://www.tddft.org/programs/octopus/wiki/index.php?title=Special:PdfPrint&page=The_Octopus_Manual" -O $@ \
	|| echo "Could not obtain the manual from http://www.tddft.org/. octopus.pdf will not be installed."
