## Copyright (C) 2012 U. De Giovannini
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.
##
##

AC_DEFUN([ACX_NFFT], [
AC_REQUIRE([ACX_FFTW])
acx_nfft_ok=no

nfft_func="nfft_init_1d"
nfft_libs="-lnfft3"

dnl Check if the library was given in the command line
AC_ARG_WITH([nfft], [AS_HELP_STRING([--with-nfft=DIR], [use NFFT library (optional) 
                                                               http://www-user.tu-chemnitz.de/~potts/nfft/index.php])],
                                                               [],
                                                               [with_nfft=no] )
case $with_nfft in
  no ) acx_nfft_ok=disable ;;
  *) LIBS_NFFT="-L$with_nfft/lib $nfft_libs" 
     CFLAGS_NFFT="-I$with_nfft/include" ;;
esac

dnl Backup LIBS and FCFLAGS
acx_nfft_save_LIBS="$LIBS"
acx_nfft_save_CFLAGS="$CFLAGS"

dnl The tests
if test $acx_nfft_ok = no; then
  AC_MSG_CHECKING([for nfft])
  # If LIBS_NFFT has been passed with --with-nfft just test this
  if test "$LIBS_NFFT"; then
    nfft_cflags="$CFLAGS_NFFT" 
    nfft_libs="$LIBS_NFFT"
    CFLAGS="$nfft_cflags $CFLAGS_FFTW"
    LIBS="$nfft_libs $LIBS_FFT"
    AC_LINK_IFELSE([AC_LANG_SOURCE([
#include "nfft3util.h"
#include "nfft3.h"
 int main(void)
 {
   nfft_plan p;
   nfft_init_1d(&p,2,3);
   nfft_finalize(&p);   
   return 1;
 }
    ])],[acx_nfft_ok=yes; CFLAGS_NFFT="$nfft_cflags"; LIBS_NFFT="$nfft_libs"], [])
  fi

  if test $acx_nfft_ok = no ; then
    AC_MSG_RESULT([$acx_nfft_ok])
  else
    AC_MSG_RESULT([$acx_nfft_ok ($LIBS_NFFT)])
  fi
fi

AC_SUBST(CFLAGS_NFFT)
AC_SUBST(LIBS_NFFT)


dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_nfft_ok" = xyes; then
   AC_DEFINE(HAVE_NFFT,1,[Defined if you have NFFT library.])
   $1
else
   if test $acx_nfft_ok != disable; then 
     AC_MSG_WARN([Could not find NFFT library.
                *** Will compile without NFFT support])
   fi
   $2
fi
])
