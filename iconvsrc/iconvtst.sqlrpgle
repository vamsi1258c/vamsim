**free
// ***************************************************************************************
// Program    : ICONV
// Description: Program to demonstrate ICONV API
//
// ***************************************************************************************

ctl-opt dftactgrp(*no) option(*srcstmt:*nodebugio:*noshowcpy);

/copy iconvff_h

dcl-ds local    likeds(QtqCode_t) inz(*likeds);
dcl-ds remote   likeds(QtqCode_t) inz(*likeds);
dcl-ds toLocal  likeds(iconv_t)   inz(*likeds);
dcl-ds toRemote likeds(iconv_t)   inz(*likeds);
dcl-s  p_inBuf  pointer;
dcl-s  p_outBuf pointer;
dcl-s  inLeft   uns(10);
dcl-s  outLeft  uns(10);
dcl-s  outLen   uns(10);
dcl-s  rc       uns(10);

dcl-s loc_data varchar(3000);
dcl-s rem_data varchar(3000);

local.ccsid  = 0   ;   // 0 = job ccsid
remote.ccsid = 1200;   // 1252 = windows latin-1

toLocal  = QtqIconvOpen( local : remote );
if toLocal.return_value = -1;
  // handle error
endif;

toRemote = QtqIconvOpen( remote: local  );
if toRemote.return_value = -1;
  // handle error
endif;

loc_data = 'O-617701-1;71511;';

// -----------------------------------------------------------------------
// Convert local (EBCDIC) data to remote (Windows-1252 ASCII)
// -----------------------------------------------------------------------

p_inBuf  = %addr(loc_data : *DATA);
inLeft   = %len(loc_data);

outLeft        = %len(rem_data: *MAX);
%len(rem_data) = outLeft;
p_outBuf       = %addr(rem_data: *DATA);

rc = iconv( toRemote
          : p_inBuf
          : inLeft
          : p_outBuf
          : outLeft );

if rc = ICONV_FAIL;
  // handle error
endif;

outLen = %len(rem_data:*MAX) - outLeft;
%len(rem_data) = outLen;


// -----------------------------------------------------------------------
//  Convert remote (Windows-1252 ASCII) data to local (EBCDIC)
// -----------------------------------------------------------------------

p_inBuf  = %addr(rem_data : *DATA);
inLeft   = %len(rem_data);

outLeft        = %len(loc_data: *MAX);
%len(loc_data) = outLeft;
p_outBuf       = %addr(loc_data: *DATA);

rc = iconv( toLocal
          : p_inBuf
          : inLeft
          : p_outBuf
          : outLeft );

if rc = ICONV_FAIL;
  // handle error
endif;

outLen = %len(loc_data:*MAX) - outLeft;
%len(loc_data) = outLen;

iconv_close(toLocal);
iconv_close(toRemote);

*inlr = *on;
