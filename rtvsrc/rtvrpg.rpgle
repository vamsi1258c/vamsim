     H    Option(*srcstmt:*NODEBUGIO)
      *******************************************************************
      *--RTVRPG  Converts RPGLE program dump to source code
      *          Created by Jim Friedman 01/26/04
      *******************************************************************
     frtvwork   if   e             disk
     fqrpglesrc uf a e             disk    rename(qrpglesrc:qrpglesrcf) usropn
     d begsource       c                   '*MODULE ENTRY'
     d endsource       c                   'TEXT DESCRIPTOR'
     d up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d i               s              3  0
     d j               s              3  0
     d k               s              3  0
     d l               s              3  0
     d numout          s              8  0
     d skip#           s              8  0
     d ctarray#        s              3  0 inz(0)
     d temp80          s             80
     d cursor          s              3  0
     d ipos            s              2  0
     d error           s              1    inz('N')
     d pmerror         s              1
     d hex             s              1a   dim(16) ctdata perrcd(16)
     d goodstuff       ds
     d  goodchr                       1    dim(32)
     d srcdta          ds
     d  srcchr                        1    dim(100)
     d lindta          ds
     d  linchr                        1    dim(200)
     d srcdatea        ds
     d  datechr                       1    dim(6)
     d hexin           ds
     d  hexchr                        1    dim(6)
     c     *entry        plist
     c                   parm      error         pmerror
     c*
     c                   open      qrpglesrc
      *--Locate beginning of source text
     c                   read      rtvwork
     c                   eval      ipos = %scan(begsource:goodstuff)
     c                   dou       ipos <> 0
     c                   read      rtvwork
     c                   eval      ipos = %scan(begsource:goodstuff)
      *--If no start string, object was not rpgle compiled with *LSTDBG
     c                   if        %eof(rtvwork) = *on
     c                   eval      error = 'Y'
     c                   seton                                        lr
     c                   return
     c                   endif
     c                   enddo
      *
      *
      *--Determine offset to beginning of statement line
     c                   eval      cursor = (32-ipos)+88
     c                   dou       cursor > 136
     c                   read      rtvwork
     c                   if        hexstuff = *blank
     c                   if        %scan('LINES':hexstuff) <> 0
     c                   exsr      $SKIP
     c                   do        skip#
     c                   eval      cursor = cursor + 32
     c                   enddo
     c                   endif
     c                   else
     c                   eval      cursor = cursor + 32
     c                   endif
     c                   enddo
      *
      *
     c                   eval      j = 6
     c                   eval      i = 32 - (cursor - 136)  + 1
      *
      *--Load rpg code (field lindta fields 1-100)
     c                   dow       %eof(rtvwork) = *off
      *
     c                   dou       i > 32
     c                   if        j < 101
     c                   eval      linchr(j) = goodchr(i)
     c                   endif
      *
      *--Load line marking (field srcdta fields 1-5)
      *  Don't load if line copied from external def (pos 5 = '=')
     c                   if        j > 105 and j < 111
     c                             and linchr(5) <> '='
     c                   eval      k = j - 105
     c                   eval      srcchr(k) = goodchr(i)
     c                   endif
      *
      *
      *--Load line dates (field srcdat)
     c                   if        j > 111 and j < 118
     c                   eval      k = j - 111
     c                   eval      datechr(k) = goodchr(i)
     c                   endif
      *
      *
     c                   eval      i = i +1
     c                   eval      j = j +1
      *
      *--Test for end of input for this source line
     c                   if        j > 136
     c                   eval      j = 1
      *
      *--Test for end of input source file data
     c                   if        %scan(endsource:lindta) <> 0
     c                   close     qrpglesrc
     c                   if        ctarray# > *zero
     c                   exsr      $FIXARRAY
     c                   endif
     c                   seton                                        lr
     c                   return
     c                   endif
      *
      *--Test for valid line date
     c                   testn                   srcdatea             30
     c                   if        *in30 = *off
     c                   eval      srcdat = *zeros
     c                   else
     c                   move      srcdatea      srcdat
     c                   endif
      *
     c*--Shift compile time arrays
     c                   if        linchr(5) <> '='
      *
      *--Count the number of compile time arrays
     c                   if        linchr(6) = 'd' or linchr(6) = 'D'
     c     lo:up         xlate     lindta        temp80
     c                   if        %scan('CTDATA':temp80) <> 0 and
     c                             linchr(7) <> '*'
     c                   eval      ctarray# = ctarray# + 1
     c                   endif
     c                   endif
      *
      *
     c                   eval      %subst(srcdta:6:95) = %subst(lindta:6)
     c                   exsr      $RMVJUNK
     c                   write     qrpglesrcf
     c                   else
     c                   clear                   qrpglesrcf
     c                   endif
     c                   endif
      *
     c                   enddo
      *--Test for end of input line
     c                   if        i > 32
     c                   eval      i = 1
     c                   endif
      *
      *--Bypass records with no data
     c                   dou       hexadr <> *blank
     c                   if        %scan('LINES':hexstuff) <> 0
     c                   exsr      $SKIP
     c                   endif
      *
      *--If 'SAME AS ABOVE' appears in input, act as if blank lines were read
     c                   if        skip# > *zero
     c                   eval      skip# = skip# - 1
     c                   eval      hexstuff = *blank
     c                   eval      hexadr   = 'FAKEIT'
     c                   iter
     c                   endif
     c*
     c                   read      rtvwork
     c                   enddo
     c                   enddo
     c*
      *******************************************************************
      *--$SKIP  skip input based upon repeating lines
      *******************************************************************
     csr   $SKIP         begsr
     c                   eval      hexin = %subst(hexstuff:24:6)
     c                   exsr      $CVTHEX
     c                   eval      skip# = numout
     c                   eval      hexin = %subst(hexstuff:8:6)
     c                   exsr      $CVTHEX
     c                   eval      skip# = (skip# - numout + 1)/32
     csr                 endsr
      *******************************************************************
      *--$RMVJUNK  remove compiler generated junk
      *******************************************************************
     csr   $RMVJUNK      begsr
      *--Get rid of '--' in c spec indicator fields
     c                   if        srcchr(6) = 'C' or srcchr(6) = 'c'
     c                   dou       k = 0
     c                   eval      k = %scan('--':srcdta:71)
     c                   if        k <> *zero
     c                   eval      srcdta = %replace('  ':srcdta:k)
     c                   endif
     c                   enddo
     c                   endif
     csr                 endsr
      *******************************************************************
      *--$FIXARRAY  shift compile time arrays
      *******************************************************************
     csr   $FIXARRAY     begsr
     c                   open      qrpglesrc
     c     *hival        setgt     qrpglesrc
      *--Position at beginning of last array
     c                   readp     qrpglesrc
     c                   dou       %eof(qrpglesrc) = *on   or
     c                             ctarray# = *zero
     c                   if        %subst(srcdta:6:2) = '**'
     c                   eval      ctarray# = ctarray# - 1
     c                   endif
     c                   readp     qrpglesrc
     c                   enddo
     c                   read      qrpglesrc
     c                   exsr      $SHIFT6L
     csr                 endsr
      *******************************************************************
      *--$SHIFT6L  shift line 6 pos to left (for compile time arrays)
      *******************************************************************
     csr   $SHIFT6L      begsr
     c                   dou       %eof(qrpglesrc) = *on
     c                   eval      %subst(srcdta:1:95) = %subst(srcdta:6:95) +
     c                             '     '
     c                   update    qrpglesrcf
     c                   read      qrpglesrc
     c                   enddo
     csr                 endsr
      *******************************************************************
      *--$CVTHEX  convert hex to numeric
      *******************************************************************
     csr   $CVTHEX       begsr
     c                   eval      numout = *zero
     c     1             do        6             k
     c                   eval      l = 1
     c     hexchr(k)     lookup    hex(l)                                 30
     c                   select
     c                   when      k = 1
     c                   eval      numout = numout + (l - 1) * 16**5
     c                   when      k = 2
     c                   eval      numout = numout + (l - 1) * 16**4
     c                   when      k = 3
     c                   eval      numout = numout + (l - 1) * 16**3
     c                   when      k = 4
     c                   eval      numout = numout + (l - 1) * 16**2
     c                   when      k = 5
     c                   eval      numout = numout + (l - 1) * 16
     c                   when      k = 6
     c                   eval      numout = numout + (l - 1)
     c                   endsl
     c                   enddo
     c
     csr                 endsr
**
0123456789ABCDEF
