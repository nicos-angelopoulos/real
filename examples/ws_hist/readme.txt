
Using Real in web servers.
----

As of Real 1.1, R can be used in Prolog servers by doing all the threading in Prolog. 
As of Real 1.3 R can be used on any thread, thanks to the good work of Samer Abdallah.
Previously all R serving was done on the main thread, as:
"Embedded R is  is designed to be run in the main thread, and all the testing is done in that context" [1]. 

To test R in web services interactively, simply consult 
==
	?- [pack(real(examples/ws_hist/ws_hist)].
==

and point your web browser to http://localhost:7171

Your R need to have ggplot2 package installed for the example to work.

To employ Real in a web server ran as a Unix service, you need to use 
library(http_unix_daemon_real) instead of library(http/http_unix_daemon)
and start the server with --wait=real. 

There is an example provided in pack(real/examples/ws_hist/).

You can test a server running this example at:
	http://stoics.org.uk:7171

The full sequence of setting up the daemon service is:
 * copy [pack(real/examples/ws_hist/swipl-ws_hist)] to /etc/init.d/  (sudo)
 * edit the above file to suit your set up
 * copy [pack(real/examples/ws_hist/ws_hist.pl)] to somewhere the above script can find 
 * start the service with  % /etc/init.d/swipl-ws_hist start
 
For more details look at pack(real/examples/ws_hist/).
and
http://stoics.org.uk/~nicos/sware/real/ws.html

As of version 1.1, Real auto-manages threading by intecepting calls to the interface predicates,
<-/2 and <-/1. If the command to be passed to R is in a thread other than main, a message is
send to thread main where the call will be served. Currently Real does not do any disambiguation
between the callers, so it is of paramount importance that the user ensures the common R environment
is sane and clean. A way to keep things tidy is to use http_session. Each session gets a unique ID
which you can use to identify R variables.

[1] http://cran.r-project.org/doc/manuals/R-exts.html#Threading-issues

---
http://stoics.org.uk/~nicos/sware/real/
Nicos Angelopoulos
London, 2014-15

