
Real 2.0
---

Real is a c-based interface for connecting R to Prolog. 
See the documentation at doc/html/real.html for more information.
There is also a paper [1] and a user's guide in doc/guide.pdf.

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

Jan Wielemaker, for reimplenting the C-interface of the prototype for v1.0,
for dots in identifiers (without flags) and web-support.


Primary reference:
[1] 

Advances in integrative statistics for logic programming
Nicos Angelopoulos, Samer Abdallah and Georgios Giamas
International Journal of Approximate Reasoning, (IJAR)
Volume 78, November 2016, pages 103-115.
http://dx.doi.org/10.1016/j.ijar.2016.06.008

[2]
Integrative functional statistics in logic programming
Nicos Angelopoulos, Vitor Costa Santos, Joao Azevedo, Jan Wielemaker, Rui Camacho and  Lodewyk Wessels
In Practical Aspects of Declarative Languages (PADL'13)
January, 2013. Rome, Italy.
http://stoics.org.uk/~nicos/pbs/padl2013-real.pdf

---
Nicos Angelopoulos 
2013-2017

Version 1.0- with Vitor Santos Costa: December, 2012.
