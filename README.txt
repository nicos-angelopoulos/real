
Real 2.0
---

Real is a c-based interface for connecting R to Prolog. 
See the documentation at doc/html/real.html for more information.
There is also a paper doc/padl2013-real.pdf and a user's guide
doc/guide.pdf, but they both refer to earlier versions of Real.

Real works on current versions of SWI and YAP.
As of version 1.1 there is support for using Real on SWI web-servers.

Licence
---
This software can be distributed under the MIT licence (see prolog/real.pl).

INSTALL
---

To install stable versions: 

 SWI-Prolog

   ?- pack_intall( real ).

 Yap 
   install Yap itself with
   configure --with-R

See doc/guide.pdf for further details on stable versions.

Git access
---

To get the latest git version goto 

   git clone https://github.com/nicos-angelopoulos/real

To install developmental versions do:

 SWI-Prolog (a)

 Install last stable real with 
 ?- pack_install( real ).
 
 replace the pack/real directory with the clone from git repository and do

 ?- pack_rebuild( real ).  % to force SWI to build from sources.

 SWI-Prolog (b)
   clone real into pl-devel/packages/real
	add this to your build script
	export EXTRA_PKGS="real"
	build SWI from sources as per usual

 Yap
   replace $YAP/packages/real with the development version you downloaded
   and rebuilt Yap with 

   configure --with-R

Test
---
 SWI

   ?- [pack('pack/real/examples/for_real')]. 
   ?- for_real.

 Yap
   ?- [examples/for_real].
   ?- for_real.


Thanks, 
Samer Abdallah, for web support improvements.

Jan Wielemaker, for dots in identifiers (without flags) and web-support.

---
Nicos Angelopoulos 
2013-2016

Version 1.0- with Vitor Santos Costa: December, 2012.
