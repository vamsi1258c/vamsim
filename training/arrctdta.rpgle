     darr_phone        s             10P 0 dim(5) ctdata perrcd(1)
     darr_name         s             10A   dim(10) ctdata perrcd(2)
     dn                s              2p 0 inz(1)
     c     1             do        5             n
     c     arr_phone(n)  dsply
     c                   enddo
      *
     c                   for       n = 1 to 10 by 1
     c*                  eval      arr_name(n) = 'shdg'
     c     arr_name(n)   dsply
     c                   endfor
     c                   eval      *inlr=*on
** CTDATA arr_phone
7000000001
8000000001
9000000001
1000000001
1100000001
** CTDATA  arr_name
Vamsi     Mahesh
Balaji    Anish
Anusha    Karan
Sudhan    Deven
Nishant   Harish
