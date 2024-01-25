**FREE
//**************************************************************************************************
//  Program     : SHIPLABEL
//  Description : Shipping Label Print and Apply Web Service
//  Author      : SenecaGlobal
//  Date Written: 02/24/21
//==================================================================================================
ctl-opt nomain option(*nodebugio:*srcstmt) pgminfo(*pcml:*module:*dclcase);

dcl-c ERROR 'ER';

dcl-ds requestFormat qualified;
   signature          char(10);
   msg_no             char(10);
   package_id_barcode char(20);
end-ds;

dcl-ds responseFormat qualified;
   signature          char(10);
   msg_no             char(10);
   package_id_barcode char(20);
   print_label        char(10000);
end-ds;


//**************************************************************************************************
// getLabelByLPN - Get shipment label by LPN from SHPRDS file
//
//**************************************************************************************************
dcl-proc getLabelByLPN export;
   dcl-pi getLabelByLPN;
      request  likeds(requestFormat);
      response likeds(responseFormat);
      httpStatus int(10);
      httpHeaders char(100) dim(10);
   end-pi;
   dcl-s inLPN zoned(12:0) inz;
   dcl-s data  char(10000) inz;

   clear httpStatus ;
   clear httpHeaders;
   httpHeaders(1) = 'Cache-Control: no-cache, no-store';
   response.signature          = request.signature;
   response.msg_no             = request.msg_no;
   response.package_id_barcode = request.package_id_barcode;

   inLPN = %dec(request.package_id_barcode:12:0);

   exec sql
      select labelData into :data
      from SHPRDS
      where LPN = :inLPN;
   if sqlcod <> 0;
      httpStatus = 204;
   else;
      response.print_label = data;
      httpStatus = 200;
   endif;


   return;

end-proc getLabelByLPN;

