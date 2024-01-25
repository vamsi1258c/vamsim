**free
// ***************************************************************************************
// Program    : ICONVFF_H COPY BOOK
// Description: ICONV - Definitions of the APIs, data structures or constants
// Date       : 08/20/2020
// ***************************************************************************************

dcl-ds QtqCode_t qualified template;
  CCSID    int(10) inz;
  ConvAlt  int(10) inz;
  SubsAlt  int(10) inz;
  ShiftAlt int(10) inz;
  InpLenOp int(10) inz;
  ErrorOpt int(10) inz;
  Reserved char(8) inz(*allx'00');
end-ds;

dcl-ds iconv_t qualified template;
  return_value int(10) inz;
  cd           int(10) dim(12) inz;
end-ds;

dcl-pr QtqIconvOpen likeds(iconv_t) extproc(*dclcase);
  toCode   likeds(QtqCode_t) const;
  fromCode likeds(QtqCode_t) const;
end-pr;

dcl-pr iconv uns(10) extproc(*dclcase);
  cd           likeds(iconv_t) value;
  inbuf        pointer;
  inbytesleft  uns(10);
  outbuf       pointer;
  outbytesleft uns(10);
end-pr;

dcl-c ICONV_FAIL CONST(4294967295);

dcl-pr iconv_close int(10) extproc(*dclcase);
  cd likeds(iconv_t) value;
end-pr;

dcl-pr QlgTransformUCSData uns(10) extproc(*dclcase);
  xformtype    int(10) value;
  inbuf        pointer;
  inbytesleft  uns(10);
  outbuf       pointer;
  outbytesleft uns(10);
  outspacereq  uns(10);
end-pr;
