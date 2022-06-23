%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Authors:       Nicos Angelopoulos, Vitor Santos Costa
%    Contributor:   Jan Wielemaker, restructuring of C code for first public version
%    Contributor:   Samer Abdallah, improvements to threads and setting R_HOME (2015)
%    Contributor:   Jan Wielemaker, 16.07.29, some compiling and tidying up in including Real in Swish, and creating new Rserve pack
%    E-mail:        Nicos Angelopoulos firstn.lastn@gmail.com
%    Copyright (C): Nicos Angelopoulos, Vitor Santos Costa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
   This program is free software; you can redistribute it and/or
    modify it under the terms of MIT license

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*/

:- module(real, [
     r/2,
     r/1,
     r_call/2,
     r_char/2,
     r_citation/2,
     r_devoff/0,
     r_devoff_all/0,
     r_end/0,  % not working currently, just prints warning
     r_is_var/1,
     r_is_var/2,
     r_library/1,
     r_remove/1,
     r_serve/0,
     r_start/0,
     r_started/1,
     r_thread_loop/0,
     r_start_server/0,
     r_call_as_server/1,
     r_version/3,
     r_wait/0,
     (<-)/1,
     (<-)/2,
     (<<-)/1,
     (<<-)/2,
     op(950,fx,<-),
     op(950,yfx,<-),
     op(950,yfx,<<-),
     op(950,xf,<<-),   % maybe this should be fx.  <<- Rvar
     op(600,xfy,~),
     % op(400,yfx,'%x%'),  % function exists
     % op(400,yfx,'%%'),   % mod
     % op(400,yfx,'%/%'),  % //
     op(400,yfx,@*@),      % %*%  matrix inner product
	op(400,yfx,@^@),      % %o%  array outer product
     op(400,yfx,@~@),      % %in% set membership

     op(400,yfx,$),
     op(400,yfx,@),
     op(800,fx,@),
     op(700,fx,!),
     op(700,fx,~),
     op(700,xfx,<=),
     % op(750,xfy,;),  % tmp? sustitute for |
     op(750,xfy,::), % tmp? sustitute for ||
     op(750,xfy,&),
     op(750,xfy,&&),
     op(400,xfy,=+),
     op(500,xfy,++), % R option appending, r_call/2
     op(100, yf, [])
     % op(100, yf, '()')
     ]).

:- multifile
     user:portray/1.
:- dynamic
     user:portray/1.
:- dynamic
     real_server_thread/1,
     r_started/0.

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).
:- use_module(library(readutil)).
:- use_module(library(debug)).

/** <module> An interface to the R statistical software.

---++ Introduction

This library enables the communication with an R process started as a shared library.
Version 1, was the result of the efforts of two research groups that have worked in parallel.
The syntactic emphasis on a minimalistic interface. Versions between 1.4 and 2.0, also 
work done by others particularly in interfacing to web applications. See credits for more details.

In the doc/ directory of the distribution there is user's guide, a published paper
and html documentation from PlDoc. There is large number of examples in `examples/for_real.pl`.

By default when the library is loaded an R object is started which will serve the
R commands. If =|current_prolog_flag(real_start,false)|=  succeeds, the R object is not loaded and the user
needs to issue r_start/0 to do that.

A single predicate (<-/2,<-/1) channels the bulk of the interactions between Prolog and R.
In addition to using R as a shared library, real uses
the c-interfaces of SWI/Yap and R to pass objects in both directions.
The usual mode of operation is to load Prolog values on to R variables and then call
R functions on these values. The return value of the called function can be either placed
on R variable or passed back to Prolog.  It has been tested extensively on current
SWI and YAP on Linux machines but it should also compile and work on MS operating systems and Macs.

Since v1.1 Real supports threads for web services and 
v1.3 it supports running an R server in any thread, not just the main thread.
The library now has the concept of a designated R server thread. By default, there is
no designated server thread, and the evaluation/execution of R expressions/commands
is done in the calling thread. This should be done in a single threaded way. A designated
server thread can come into existence in one of three ways:
   1. By starting a dedicated server thread using r_start_server/0.
   2. By running r_thread_loop/1 in any thread. This will run until a message to quit
      the thread is received by executing r(r_thread_loop_stop) or <- r_thread_loop_stop
      in any thread.
   3. By running any goal G as r_call_as_server(G). While G is running, the thread that
      it is running in becomes the designated server thread, and G should call r_serve/0
      periodically to answer any R requests that accumulate.
While there is a designated server thread, a call to r/1, r/2, (<-)/1 or (<-)/2 in any
thread results in the request being posted to the server thread and the current thread
blocking until a reply is received. As of July 2016 SWI-Prolog also has an alternative
pack (pack(rserve_client)) which works with Rserve and Swish.

The main modes for utilising the interface are
==
     <- +Rexpr
     <- +Rvar
==

     Print  Rvar or evaluate expression Rexpr in R
==
     +Rvar   <- +PLdata
     +Rexpr  <- +PLdata
     -PLvar  <- +Rvar
     -PLvar  <- +Rexpr
     +Rexpr1 <- +Rexpr2
==

Pass Prolog data to R, pass R data to Prolog or assign an R expression to
an assignable R expression.

---++ Testing

There is a raft of examples packed in a sinlge file that test the library.

==
     ?- [pack(real/examples/for_real)].

     ?- for_real.

     ?- edit( pack(real/examples/for_real) ).
==

---++ Syntax

There are syntactic conventions in R that make unparsable prolog code.
Notably function and variable names are allowed to contain dots, square brackets are used
to access parts of vectors and arrays and functions are allowed empty argument tuples.
We have introduced relevant syntax which allows for easy transition between prolog and R.
Prolog constructs are converted by the library as follows:


     * =|..|= within atoms  ->  =|.|= (ex. =| as..integer(c(1,2,3)) ->  as.integer(c(1,2,3))|= )
     * =|^[]|=  after atoms -> =|[]|= (ex. =|a^[2] -> a[2] |=)
     * =|(.)|= at the end of atoms that are known R functions -> =|()|=  (ex. =|dev..off(.) -> dev.off()|= )
     * =|[]|= -> c() (which equal to R's NULL value)
     * ( f(x) :-  (..))   -> f(x) (...)
     * Lists of lists are converted to matrices. All first level lists must have the same length.
     * Filenames must be given as Prolog strings.
     * R specific operators (eg. %*% should be quoted in Prolog.
     * + prepends strings, both for (Prolog) codes and atoms: +"String" and +'String'
     * If you want to ensure that you do not quote a string, use -"String".
     * Expressions that pose difficulty in translation can always be passed as unquoted Prolog atoms or strings.
     * since  0:1:2  foo()  is valid syntax:  =|<- dev..off() |= works now (with no need for dev..off(.))
     * since  0:1:2  mat[1] is valid syntax:  =|m[1] <- 4|= works now (with no need for m^[...])
     * since  2:0:0  dots are allowed in atoms even without prolog_flag( allow_dot_in_atom, true ).

---++ Data transfers

R vectors are mapped to prolog lists and matrices are mapped to nested lists.
The convention works the other way around too.

There are two ways to pass prolog data to R. The more efficient one is by using
==
 Rvar <- PLdata
==

Where Pldata is one of the basic data types (number,boolean) a list or a c/n term.
This transfers via C data between R and Prolog. In what follows atomic PLval data
are simply considered as singleton lists.
Flat Pldata lists are translated to R vectors and lists of one level of nesting to R matrices
(which are 2 dimensional arrays in R parlance). The type of values of the vector or matrice is
taken to be the type of the first data element of the Pldata according to the following :

     * integer -> integer
     * float   -> double
     * atom    -> char
     * boolean -> logical

Booleans are represented in prolog as true/false atoms.
Currently arrays of aribtrary dimensions are not supported in the low-level interface.
Note that in R a scalar is just a one element vector.  When passing non-scalars the
interface will assume the type of the object is that of the first scalar until it encounters
something different.
Real will currently re-start and repopulate partial integers for floats as illustrated
below:

==
r <- [1,2,3].         % pass 1,2,3 to an R vector r
R <- r.               % pass contents of R vector r to Prolog variable R
R = [1, 2, 3].

i <- [1,2,3.1].       % r is now a vector of floats, rather than integers
I <- i.
I = [1.0, 2.0, 3.1].


==

However, not all possible "corrections" are currently supported. For instance,

==
?- c <- [a,b,c,1].
ERROR: real:set_r_variable/2: Type error: `boolean' expected, found `a'
==

In the data passing mode we map Prolog atoms to R strings-

==
?- x <- [abc,def].
true.

?- <- x.
[1] "abc" "def"
true.

?- X <- x.
X = [abc, def].

==

In addition, Prolog data can be passed through the expression mechanism.
That is, data appearing in an arbitrary R expression will be parsed and be part of the long
string that will be passed from Prolog to R for evaluation.
This is only advisable for short data structures. For instance,

==
tut_4a :-
    state <- c(+"tas", +"sa",  +"qld", +"nsw", +"nsw"),
    <- state.

tut_4b :-
    state <- c(+tas, +sa,  +qld, +nsw, +nsw),
    <- state.
==

Through this interface it is more convenient to be explicit about R chars by Prolog prepending
atoms or codes with + as in the above example.

The Prolog atoms '$NaN' and '' are passed to NA values in R. '$NaN' is the bidirectional value,
'' is only understood in the Prolog -> R direction as it is useful for passing missing values
from CSV read matrices.

==
nan_ex :-
    x <- [c(1,2,''),c(3,4,'$NaN')],
    X <- x,
    write( x(X) ), nl.

?- nan_ex.
x( [[1, 2, '$NaN'], [3, 4, '$NaN']] )

==

---++ Other predicates

Use r_citation/2 to access publication information about the interface.
Although the original name was R..eal, when citating please use Real as
the name for this library.

The library listens to
==
?- debug(real).
?- nodebug(real).
==

Predicate <<-/2 is a shorthand that ensures that the R variable on the left is
fresh/new at the time of call, and <<-/1 blanks R variable out (r_remove/1).

---++ Examples

==

?- e <- numeric(.).
yes
?- e^[3] <- 17.
yes
?- e[3] <- 17.
yes
?- Z <- e.
Z = ['$NaN','$NaN',17.0]
?- e^[10] <- 12.
yes
?- Z <- e.
Z = ['$NaN','$NaN',17.0,'$NaN','$NaN','$NaN','$NaN','$NaN','$NaN',12.0]

rtest :-
     y <- rnorm(50),               % get 50 random samples from normal distribution
     <- y,                         % print the values via R
     x <- rnorm(y),                % get an equal number of normal samples
     <- x11(width=5,height=3.5),   % create a plotting window
     <- plot(x,y)                  % plot the two samples
     r_wait,                       % wait for user to hit Enter
     % <- dev..off(.).             % old syntax, still supported
     <- dev..off().                % close the plotting window. foo() now acceptable in supported Prologs

tut6 :-
     d <- outer(0:9, 0:9),
     fr <- table(outer(d, d, "-")),
     <- plot(as..numeric(names(fr)), fr, type="h", xlab="Determinant", ylab="Frequency").

tut4b :-
     state <- [tas,sa,qld,nsw,nsw,nt,wa],
     statef <- factor(state),
     incmeans <- tapply( c(60, 49, 40, 61, 64, 60, 59), statef, mean ),
     <- incmeans.

logical :-
     t <- [1,2,3,4,5,1],
     s <- t==1,
     <- s,
     S <- s,
     write( s(S) ), nl.

==

---++ Info

@author     Nicos Angelopoulos
@author     Vitor Santos Costa
@version    2:3:0, 2022/6/23, rotten_bins
@license    MIT
@see        http://stoics.org.uk/~nicos/sware/real
@see        ?- pack(real/examples/for_real), for_real
@see        pack(real/doc/real.html)
@see        pack(real/doc/guide.pdf)
@see        pack(real/doc/padl2013-real.pdf)
@see        http://www.r-project.org/

*/

%%%

init_r_env :-
     getenv('R_HOME',Path),
     % done, except if in windows...
     \+ current_prolog_flag(windows, true),
     !,
     debug( real, 'Found R_HOME: ~a', [Path] ).
:- if(current_predicate(win_registry_get_value/3)).
init_r_env :-
     % windows is windows
        current_prolog_flag(windows, true),
     ( HKEY='HKEY_LOCAL_MACHINE/Software/R-core/R';
          HKEY='HKEY_CURRENT_USER/Software/R-core/R' ),
        catch(win_registry_get_value(HKEY,'Current Version', Version),_,fail),
     !,
     atomic_list_concat([HKEY,Version],'/',SecondKey),
     win_registry_get_value(SecondKey,'InstallPath', RPath), !,
     setenv('R_HOME',RPath), % this probably does not help (at least not XPs)
     % now we need to have the DLL in our path
     % nicos: although on xp it seems that path has to already be set.
     ( current_prolog_flag(address_bits, 64) ->
          Psf = '\\bin\\x64'
          ;
          Psf = '\\bin\\i386'
     ),
     atomic_list_concat( [RPath,Psf], ToR ),
     install_in_ms_windows(ToR).
:- endif.
init_r_env :-
     % SA: this should work whenever R is in the path
     absolute_file_name(path('R'),_,[access(execute)]), !,
     setup_call_cleanup( open(pipe('R RHOME'),read,Stream),
                         read_line_to_codes(Stream,Codes),
                         close(Stream)),
     atom_codes(Home,Codes),
     debug( real, 'Setting R_HOME to: ~a', [Home] ),
     setenv('R_HOME',Home).
init_r_env :-
        current_prolog_flag(unix, true),
     % typical Linux 64 bit setup (fedora)
     current_prolog_flag(address_bits, 64),
     Linux64 = '/usr/lib64/R',
     exists_directory(Linux64), !,
     debug( real, 'Setting R_HOME to: ~a', [Linux64] ),
     setenv('R_HOME',Linux64).
init_r_env :-
     current_prolog_flag(unix, true),
     % typical Linux  setup (Ubuntu)
     Linux32 = '/usr/lib/R',
     exists_directory( Linux32 ), !,
     debug( real, 'Setting R_HOME to: ~a', [Linux32] ),
     setenv('R_HOME',Linux32).
% nicos, fixme: Linux multilib ?

init_r_env :-
     % typical MacOs setup
     exists_directory('/Library/Frameworks'), !,
     install_in_osx.
init_r_env :-
     absolute_file_name( path('R'), This,
                [ extensions(['',exe]),
                  access(execute),
                  file_errors(fail) % Wouter Beek, 14.03.18
                ] ),
     dirpath_to_r_home( This, Rhome ),
     exists_directory( Rhome ), !,
     debug( real, 'Setting R_HOME to bin relative: ~a', [Rhome] ),
     setenv('R_HOME',Rhome).
init_r_env :-
     throw( real_error(r_root) ).

% track down binary through symbolic links...
%
dirpath_to_r_home( This0, Rhome ) :-
     read_link(This0, _, This), !,
     dirpath_to_r_home( This, Rhome ).
dirpath_to_r_home( This, Rhome ) :-
     file_directory_name( This, R1 ),
     file_base_name(R1, Execdir) ->
     ( Execdir == bin ->
       Rhome = R1
     ;
       % windows with multiple binaries
       file_directory_name( R1, R2 ),
       file_base_name(R2, bin),
       file_directory_name( R2, Rhome )
     ).

r_home_postfix( 'lib64/R' ) :-
     current_prolog_flag(address_bits, 64).
r_home_postfix( 'lib/R' ).

to_nth( [To|T], To, T ) :- !.
to_nth( [_H|T], To, Right ) :-
     to_nth( T, To, Right ).

% nicos: This should become the standard way.  2013/01/02.
:- if(current_predicate(win_add_dll_directory/1)).
install_in_ms_windows( ToR ) :-
     debug( real, 'Setting up ms-wins dll directory: ~a', [ToR] ),
     win_add_dll_directory( ToR ),
     install_in_ms_windows_path( ToR ).
:- else.
install_in_ms_windows(RPath) :-
     install_in_ms_windows_path( RPath ).
:- endif.

install_in_ms_windows_path(RPath) :-
     getenv('PATH',OPath),
     atomic_list_concat([OPath,';',RPath],Path),
     % if you have problems with R associated dlls, you might also want to add:
     % atomic_list_concat([IPath,';',RPath,'\\modules\\i386'],Path),
     debug( real, 'Changing wins path to: ~a', [Path] ),
     setenv('PATH',Path).

install_in_osx :-
     current_prolog_flag(address_bits, 64),
     Mac64 = '/Library/Frameworks/lib64/R',
     exists_directory(Mac64), !,
     debug( real, 'Setting R_HOME to: ~a', [Mac64] ),
     setenv('R_HOME',Mac64).
install_in_osx :-
     % typical MacOs setup
     MacTypical = '/Library/Frameworks/R.framework/Resources',
     exists_directory(MacTypical), !,
     debug( real, 'Setting R_HOME to: ~a', [MacTypical] ),
     setenv('R_HOME', MacTypical).
install_in_osx :-
     LastMac = '/Library/Frameworks/lib/R',
     ( exists_directory(LastMac) ->
     debug( real, 'Setting R_HOME to: ~a', [LastMac] )
          ;
          debug( real, 'Setting R_HOME to non-existing: ~a', [LastMac] )
     ),
     setenv('R_HOME', LastMac ).

% interface predicates

%% r_start is det.
%
%  Start an R object. This is done automatically upon loading the library,
% except if =|current_prolog_flag( real_start, false)|= succeeds.
% Only 1 instance should be started per Prolog session.
% Calls to the predicate when the R object is loaded and connected to succeed
% silently but have no useful side-effects.
r_start :-
     r_started(false), !,
     swipl_wins_warn,
     init_r_env,
     use_foreign_library(foreign(real)),
     init_r,
     assert( r_started ).
r_start :-
     r_started(true), !,
     print_message(informational,real_error(r_already_started)).

% SA: Disabled for now, as it does not seem to have any effect, and
% calling r_start after r_end results in a crash.
% nicos: Made this print a warning instead.
%% r_end.
%
%    End the connection to the R object.
%    Currently this only prints a warning.
%
% r_end :-
%      stop_r,
%      retractall( r_started ).
%
r_end :-
     print_message( informational, real_error(stop_r_is_buggy) ).

%% r_started(-F:boolean) is det.
%
%  Unifies F with true if R has been started or false if not.
r_started(F) :- r_started -> F=true; F=false.

%%     '<-'(+Rvar).
%%     '<-'(+Rexpr).
%
%         If Rvar is an atom and a known R object, then print Rvar on R.
%         Else treat the input as an R expression and pass it on R for interpretation.
%        (Throws result away, if expression is not a <- expression itself).
%
'<-'(X) :-
     r(X).

%%  '<-'(+Rexpr, +PLdata ).
%%  '<-'(-PLvar, +Rexpr ).
%%  '<-'(+Rexpr1, +Rexpr2 ).
%
%  Pass Prolog data PLdata to Rvar. PLdata is a term that is one of:
%  an atomic value, flat list or list of depth 2. This mode uses the C-interface to pass
% the value to an R variable.
%
%  Pass PLdata to an assignable R expression.
%
%  Pass Rvar to PLvar variable via the C-interface.
%
%  Evaluate Rexpr and store its return value to PLvar.
%
%  Pass Rexpr1 <- Rexpr2 to R.
%
%  Note that all Rexpr* are first processed as described in the section about syntax before passed to R.
% Real also looks into Rexpressions and passes embeded lists to hidden R variables in order
% to pass large data efficiently.
%
%  c/n terms are recognised as PLdata
% if and only if they contain basic data items in all their arguments that can be
% cast to a single data type. This builds on the c() function of R that is a basic
% data constructor. Currently c/n terms are not recognised within nested expressions.
% But a mechanism similar to the hidden variables for Prolog lists in expressions should
% be easy to implement.
%
'<-'(X,Y) :-
     r(X,Y).

%% <<-( Rvar ).
%
% Nick name for r_remove( Rvar ).
%
% See r_remove/1.
'<<-'( X ) :-
     r_remove( X ).

%% <<-( +Rv, +Expr ).
%
% True iff Rv is a undefined R variable and Rv <- Expr succeeds.
% If Rv is not an atom or if its an atom that corresponds to an R variable the predicate errors.
%
% See r_new/1 for a predicate that fails instead in a similar context.
%
%==
% ?- x <<- [1,2,3].
% true.
%
% ?- x <<- [1,2,3].
% ERROR: First argument of <<- exists as R variable: x.
%==
'<<-'(X,Y) :-
     r_new(X),
     !,
     r( X, Y ).
'<<-'(X,_Y) :-
     atom( X ),
     r_is_var(X),
     !,
     throw( real_error(r_new_exists(X)) ).
'<<-'(X,_Y) :-
     \+ atom( X ),
     !,
     throw( real_error(r_new_var(X)) ).
'<<-'(X,_Y) :-
     throw( real_error(r_new_inconsistent(X)) ).

%% r( R )
%
%   Nickname for <-(R).
%
r( R ) :-
     var( R ),
     !,
     % fixme: print better message
     throw(error(instantiation_error,r/1)).
r( R ) :-
     real_server_thread( Server ),
     real_thread_self( Self ),
     Self \== Server,
     !,
     r_thread( Server, Self, r(R) ).
r( R ) :-
     r_term( R ).

r_term( Lib ) :-
     Lib = library(R),
     !,
     r_library( R ).
r_term( RvarIn ) :-
     (  rvar_identifier(RvarIn,_,RvarCs) ->
        true
        ; (atom(RvarIn),atom_codes(RvarIn,RvarCs))
     ),
     !,
     atom_codes('print( ', PrintOpen), % JW: I think we should be using atoms
     atom_codes(' )', PrintClose),     % JW: all along
     append([PrintOpen,RvarCs,PrintClose], CmdCodes),
     atom_codes( Cmd, CmdCodes ),
     r_send( Cmd ).
r_term( A ++ B ) :-
     !,
     r_call( A, B ).
r_term( Term ) :-
     rexpr( Term, TmpRs, R ),
     !,
     r_send( R ),
     maplist( r_remove, TmpRs ).
r_term( _Other ) :-
     % fixme: print "proper" error
     write( user_error, 'Cannot use input to <-/1.' ), nl, nl,
     fail.

%%     r( ?L, +R ).
%
%    Nickname for <-(L,R).
%
r( A, B ) :-
     real_server_thread( Server ),
     real_thread_self( Self ),
     Self \== Server,
     debug( real, 'Calling from thread:~p', Self ),
     !,
     r_thread( Server, Self, r(A,B) ).
     % thread_send_message( main, real_call(Caller,Real) ),
     % thread_get_message( Caller, real_ply(Ball,Real) ),
     % fixme: we should be able to write the caught Ball here, except if it is
     % is thread related, in which case possibilities are probably also limited

r( A, B ) :-
     r_assign( A, B ).

/*
r( A, B ) :-
     current_prolog_flag( real, thread ),
     !,
     debug( real, 'Using R on thread',  [] ),
     r_thread( r(A,B) ).
     */
r_assign( C, A ++ B ) :-
     !,
     r_call( A, [rvar(C)|B] ).
r_assign( Plvar, RvarIn ) :-
     var(Plvar),
     rvar_identifier( RvarIn, RvarIn, _ ),
     !,
     debug( real, 'Assigning to Prolog variable R variable ~a',  [RvarIn] ),
     robj_to_pl_term( RvarIn, Plvar ).
%   Plvar <- Rexpr.
r_assign( Plvar, Rexpr ) :-
     var(Plvar),
     rexpr( Rexpr, TmpRs, R ),
     !,
     debug( real, 'Assigning to Prolog variable R expression ~a',  [R] ),
     atom_codes( R, Rcodes ), % fixme, make the following take atoms
     rexpr_to_pl_term( Rcodes, Plvar ),
     maplist( r_remove, TmpRs ).
%  Rvar <- Plval.
r_assign( RvarIn, PlrExpr ) :-
     assignment( PlrExpr, RvarIn ),
     !.
%  Rexpr1 <- Rexpr2
r_assign( LRexpr, RRexpr ) :-
     rexpr('<-'(LRexpr,RRexpr),TmpRs,R),
     !,
     r_send( R ),
     maplist( r_remove, TmpRs ).
r_assign( _Plvar, _Rexpr ) :-
     write( user_error, 'Cannot decipher modality of <-/2. \n ' ), nl,
     fail.

% r_start_server is det.
%
% Starts a new thread running r_thread_loop/0 as an R server.
% The created thread is given an alias of 'real' and is detached.
% If more control over thread creation is required, then you can
% create the thread yourself and call r_thread_loop within it.
%
% Once started, any calls to r/1, r/2, (<-)/1, or (<-)/2 work by passing
% a message to the server thread and waiting for a response.
% See r_call_as_server/1 for an alternative approach to multithreaded
% R programming.
%
% @throws real_error(server_already_running(ThreadId)) if another thread
% has already been designated as an R server.
r_start_server :-
   r_check_no_server,
   thread_create(r_thread_loop, _, [alias(real),detached(true)]).


% r_call_as_server(Goal).
%
% Calls Goal with the current thread designated as an R serving thread. This
% means that any other thread that calls an R goal will send a request to this thread.
% By using this predicate, you agree to check for and execute
% and R requests by calling r_serve/0 periodically.
% While this goal is running, any attempt to create a new R server thread will
% result in an exception.
%
% @throws real_error(server_already_running(ThreadId)) if another thread
% has already been designated as an R server.
r_call_as_server(Goal) :-
     r_check_no_server,
     thread_self( Me ),
     debug(real, 'Running as R server on ~w: ~q...',[Me,Goal]),
     setup_call_cleanup(
        assert( real_server_thread(Me) ),      Goal,
        retractall( real_server_thread(_) ) ).

r_check_no_server :-
   (  real_server_thread(TID)
   -> throw(real_error(server_already_running(TID)))
   ;  true
   ).


%% r_thread_loop is det.
%
% Starts a loop that serves R calls received from
% <-/1 and <-/2 calls from other threads.
% It can be run on any thread as long as no other thread is
% running an R serving thread. If there is, an exception is thrown.
% To stop it, query from any thread in the pool:
% ==
%    <- r_thread_loop_stop.
% ==

r_thread_loop :-
     r_call_as_server( r_thread_loop_body ).

r_thread_loop_body :-
     thread_get_message( Mess ),
     r_thread_message( Mess ).

r_thread_message( quit ) :-
     !,
     halt(0).
r_thread_message( real_call(Caller,Goal) ) :-
     debug( real, 'In r_thread_loop got ~p, from ~p', [Goal,Caller] ),
     r_thread_serve( Goal, Caller ).

r_thread_serve( r(r_thread_loop_stop), Caller ) :-
     % debug( real, 'In r_thread_loop2 got ~p from ~p', [Goal,Caller] ),
     % Goal =.. [Name|Args],
     % debug( real, 'Name ~p args ~p', [Name,Args] ),
     % Goal = <-(r_thread_loop_stop),
     !,
     debug( real, 'Caught stop_loop signal from caller: ~p', Caller ),
     thread_send_message( Caller, real_ply(yes,r(r_thread_loop_stop))).
r_thread_serve( Goal, Caller ) :-
     reify( Goal, Result ),
     debug( real, 'Called ~p, result ~p', [Goal,Result] ),
     thread_send_message( Caller, real_ply(Result,Goal) ),
     r_thread_loop_body.

%% r_serve.
%
%  Serves any R calls that are waiting on the thread queue.
%  The queue is populated by calls to <-/1 and <-/2 that are called
%  on other threads. The predicate succeeds if there are no calls
%  in the queue.
%
%  This predicate *must* be called in the context of r_call_as_server/1;
%  this is required to ensure that the current thread is designated as
%  an R server thread, so that R evaluations from other threads are
%  properly redirected to this thread.
%
%  @throws real_error(no_server_thread) if no thread has been designated a server thread.
%  @throws real_error(thread_server_mismatch(T1,T2) if r_serve/0 is called on thread T1 but the designated server thread is T2.
r_serve :-
     thread_self( Me ),
     ( real_server_thread( Server ) -> true; throw(real_error(no_server_thread))),
     ( Server\=Me -> throw(real_error(server_thread_mismatch(Me,Server))); true),
     thread_peek_message( _G),
     !,
     thread_get_message( real_call(Caller,Goal) ),
     debug( real, 'In main got ~p, from ~p', [Goal,Caller] ),
     reify( with_mutex( real, Goal ), Result ),
     debug( real, 'Called ~p, result ~p', [Goal,Result] ),
     thread_send_message( Caller, real_ply(Result,Goal) ),
     r_serve.
r_serve.

r_thread( Eval, Caller, Real ) :-
     % thread_self(Caller),
     debug( real, 'Sending call ~p from caller ~p to evaluator ~p', [Real,Caller,Eval] ),
     thread_send_message( Eval, real_call(Caller,Real) ),
     thread_get_message( Caller, real_ply(Result,Real) ),
     debug( real, 'Caller ~p received goal ~p and got result ~p', [Caller,Real,Result] ),
     reflect( Real, Result ).

reify( Goal, Result) :-
   (  catch( (Goal,Result=yes), Ex, Result=ex(Ex) ) -> true
   ;  Result = no
   ).

reflect(_,yes) :- !.
reflect(Real,ex(Ex)) :- throw(real_error(thread(Real,Ex))).

%% r_is_var(+Rvar).
%         True if Rvar is an atom and a known variable in the R environment.
r_is_var( Rvar ) :-
     r_is_var( Rvar, _ ).

%% r_is_var(+Rvar,-RvarAtom).
%         True if Rvar is a term and a known variable in the R environment.
%         RvarAtom is the atomic representation of the Rvar term.
%
r_is_var( RvarIn, Rvar ) :-
     atom(RvarIn), !,
     is_r_variable(RvarIn),
     RvarIn = Rvar.
r_is_var( RvarIn, Rvar ) :-
     rvar_identifier( RvarIn, Rvar, _RvarAtom ),
     is_r_variable( Rvar ),
     rexpr( mode(Rvar), [], Rmode ),
     atom_codes( Rmode, RmodeCs ), % fixme, make the following take atoms
     rexpr_to_pl_term( RmodeCs, Plmode ),
     RvarModes  = [character,complex,list,logical,'NULL',numeric,raw,'S4'],
     memberchk( Plmode, RvarModes ).

%%     r_char( +Atomic, +RcharAtom ).
%
%   Wrap an atomic value with double quotes so it can pass as an R char type.
%   This is more or less obsolete. You can use +Atomic directly in R expressions.
%
r_char( Atomic, Rchar ) :-
    atomic( Atomic ),
    !,
    atomic_list_concat( ['"',Atomic,'"'], Rchar ).

%%     r_devoff.
%  Close the current plot devise without any reporting. Short for <- invisible('dev.off'()').
r_devoff :-
     <- invisible(-'dev.off()').

%% r_devoff_all.
%
% Close all open devices.
%
r_devoff_all :-
     Dev <- 'dev.cur()',
     Dev > 1,
     !,
     r_devoff,
     r_devoff_all.
r_devoff_all.

%% r_new( +Rvar ).
%
% True iff Rvar is an atom and not a current R variable.
% The predicate fails silently otherwise.
%
% See <<-/2 for a version that throws errors in a similar scenario.
%
%==
% ?- r_new( x ).
% true.
% ?- x <- [1,2,3].
% true.
% ?- r_new( x ).
% fail.
% ?- x <<- true.
%
%==
%
r_new( Rv ) :-
     atomic( Rv ),
     \+ r_is_var( Rv ).

%% r_wait
%         Currently only waiting for Return to be pressed.
%
r_wait :-
     write('Press Return to continue...'), nl,
     read_line_to_codes(user_input, _).

%% r_library( +Rlib ).
%
% Load Rlib while respecting prolog_flag/2 real_suppress_lib_messages.
%
% By default and when the flag is not defined messages are suppressed
% by wrapping the call to R's suppressPackageStartupMessages().
%
% If you want the messages, use
%==
% ?- set_prolog_flag( real_suppress_lib_messages, false ).
%==
%
% The predicate first looks into all subdirs of R_LIB_REAL
% for Rlib, Rlib.r and Rlib.R which allows to use local implementations
% rather than library packages. This is useful if you have made changes
% to a publically available R package that has a single file entry point.
% You can then use the local version for your purposes but allow others
% to also use your Real code with the puablic R function without any changes
% to the interface calls. The usual scenario is that the local version
% has a couple of extra arguments that specialises usage. Interface predicates
% to the R package can happily thus work with either version.
%
% For instance, assume file '/home/user/r/lib/pheatmap.r' is a local file
% that can be independently sourced and corrensponds to the main function file
% of R's package pheatmap. Then the following code will source the local copy
% rather than look for the package installed via R.
%==
% ?- setenv( 'R_LIB_REAL', '/home/user/r/lib' ), debug(real), r_library(pheamap).
% % Sending to R: source("/home/nicos/islp/r/lib/pheatmap.R")
%==
% If you want to use locally installed packages include their
% root location to R_LIB_USER (as per R documentation).
%
% Examples:
%==
%  ?- r_library( ggplot2 ).
%  ?- r_library( "ggplot2" ).
%  ?- r_library( [ggplot2,goProfiles] ).
%  ?- debug( real ).
%  ?- <- library("ggplot2").
%  % Sending to R: suppressPackageStartupMessages(library(ggplot2))
%  ?- set_prolog_flag( real_suppress_lib_messages, false ).
%  ?- <- library("ggplot2").
%  % Sending to R: library(ggplot2)
%==
%
% <- library(Rlib) also re-directs here. These are the best ways
% to include R libraries from within Real. Rlib is allowed to be atomic or
% a string, or a list of atoms each corresponding to an R library name.
%
r_library( Rlib ) :-
     current_predicate(string/1),
     string( Rlib ),
     !,
     atom_string( RlibAtm, Rlib ),
     r_library( RlibAtm ).
r_library( Rlib ) :-
     getenv( 'R_LIB_REAL', RlibRealPath ),
     atomic_list_concat( RlibDirs, ':', RlibRealPath ),
     member( Rdir, RlibDirs ),
     member( Ext, ['','r','R'] ),
     file_name_extension( Rlib, Ext, Rbase ),
     directory_file_path( Rdir, Rbase, Rfile ),
     exists_file( Rfile ),
     !,
     <- source( +Rfile ).

r_library( Rlib ) :-
     current_prolog_flag( real_suppress_lib_messages, false ),
     !,
     r_library_codes( Rlib, '', '', Rcodes ), % fixme to atom
     atom_codes( R, Rcodes ),
     r_send(R).
r_library( Rlib ) :-
     Pre = 'suppressPackageStartupMessages(',
     r_library_codes( Rlib, Pre, ')', Rcodes ),
     atom_codes( R, Rcodes ),
     r_send( R ).

r_library_codes( Rlib, Pre, Post, Rcodes ) :-
     ( is_list(Rlib) -> Rlib=Rlibs; Rlibs = [Rlib] ),
     atomic_list_concat( Rlibs, ',', RlibsAtm ),
     atomic_list_concat( [Pre,'library(',RlibsAtm,')',Post], RlibCallAtm ),
     atom_codes( RlibCallAtm, Rcodes ).

%% r_version( -Version, -Date, -Note ).
%
% Version and release Date (data(Y,M,D) term). Note is either a note or nickname
% for the release. In git development sources this is set to <Something>_dev.
%
%==
% ?- r_version( V, D, N ).
% V = 2:3:0,
% D = date(2022, 6, 23),
% N = rotten_bins.
%==
%
%@version 2:2:0, 2022/6/21, new_bins
%@version 2:1:0, 2020/5/29, swi8_2
%@version 2:0:0, 2016/9/5, ijar
%@version 1:5:0, 2016/1/23, j_review
%@version 1:4:0, 2015/5/24, configurable
%@version 1:3:0, 2015/5/3,  collaborative
%@version 1:2:0, 2015/1/2,  regardless
%@version 1:1:0, 2013/3/24, thankless_task
%@version 1:0:0, 2013/12/6, sinter_class
%@version 0:1:2, 2013/11/3, the_stoic
%@version 0:1:0, 2012/12/26,oliebollen
r_version( 2:3:0, date(2022,6,23), rotten_bins ).

%% r_citation( -Atom, -Bibterm ).
%
% Although the original name was R..eal, when citing please use Real as the name for this library.
%
% This predicate succeeds once for each publication related to this library.
% Atom is the atom representation % suitable for printing while Bibterm 
% is a bibtex(Type,Key,Pairs) term of the same publication. 
% Produces all related publications on backtracking.
%
r_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom = 'Advances in integrative statistics for logic programming\nNicos Angelopoulos, Samer Abdallah and Georgios Giamas \nInternational Journal of Approximate Reasoning, 8:103-115, 2016\nhttp://dx.doi.org/10.1016/j.ijar.2016.06.008.',
    Type = article,
    Key  = 'AngelopoulosN+2016',
    Pairs = [
               author = 'Nicos Angelopoulos, Samer Abdallah and Georgios Giamas',
               title  = 'Advances in integrative statistics for logic programming',
               journal = 'Journal of Approximate Reasoning',
               year = 2016,
               volume = 78,
               month = 'November',
               pages = '103-115',
               pdate = 'online:2016/7/5',
               url   = 'http://dx.doi.org/10.1016/j.ijar.2016.06.008'
     ].

r_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom = 'Integrative functional statistics in logic programming \nNicos Angelopoulos, Vítor Santos Costa, Joao Azevedo, Jan Wielemaker, Rui Camacho and Lodewyk Wessels \nProc. of Practical Aspects of Declarative Languages (PADL 2013). Accepted (January, 2013. Rome, Italy).',
    Type = inproceedings,
    Key  = 'AngelopoulosN+2012',
    Pairs = [
               author = 'Nicos Angelopoulos and Vitor Santos Costa and Joao Azevedo and Jan Wielemaker and Rui Camacho and Lodewyk Wessels',
               title  = 'Integrative functional statistics in logic programming',
               booktitle = 'Proc. of Practical Aspects of Declarative Languages}',
               year = 2013,
               month = 'January',
               address = 'Rome, Italy',
               url     = 'http://stoics.org.uk/~nicos/pbs/padl2013-real.pdf'
     ].

%% r_remove( Rvar ).
%
% Remove Rvar from R's workspace (<- remove(Rvar)).
%
r_remove( Plvar ) :-
     <- remove( Plvar ).

r_call_defaults( Defs ) :-
     Defs = [ call(true), fcall(_), outputs(false), stem(real_plot) ].

%% r_call( +Fun, +Opts ).
%
% Construct and possibly call an R function.
% Fun can be an atom or a compound, eg plot, or plot(width=3).
% The predicate also supports multiple output destinations.
%
% Opts a single or list of the following:
%  * Ropt=Rarg
%     =/2 terms in Opts are added to the function call
%  * call(Call=true)      
%     whether to call the constructed function
%  * debug(Dbg=false)
%     turn on debug(real) and restore at end of call
%  * fcall(Fcall)
%     returns the constructed Fcall
%  * outputs(Outs=false)
%     a single or list of [false,x11,pdf] also terms of those (eg x11(width=7))
%  * post_call(Post)
%     call this after the function call. this can be an arbitrary callable including another <-/2 or r_call/2
%  * rmv(Rmv=false)
%     when Rvar is given, should it be removed from R workspace at end? (see r_remove/1)
%  * rvar(Rvar)
%     when given call is expanded to Rvar <- Fcall, else <- Fcall is called
%  * stem(Stem=real_plot)
%     stem to use for output files
%
% Only the first Ropt=Rarg for each matching Ropt is used. This is also the case
% for =pairs in args of Func. These are pre-pended for the check, so they always have
% precedence.
%
%==
% ?- r_call( plot([1,2,3]), [debug(true)]  ).
% ?- <- plot(c(1,2,3)) ++ debug(true).
% ?- <- plot(c(1,2,3)) ++ xlab=+an_xlab
%==
%
r_call( FPre, ArgS ) :-
     to_list( ArgS, Args ),
     ( memberchk(debug(true),Args) -> debug(real); true ), % fixme: turn-off again
     FPre =.. [Fun|FPreList], % fixme: ? test plot, plot() & plot(c(1,2,3))
     r_call_defaults( Defs ),
     partition( eq_pair, FPreList, FPreEqPairs, FPreRArgs ),
     flatten( [FPreEqPairs,Args,Defs], Opts ),
     options_equals_pairs( Opts, Rpairs ),
     append( FPreRArgs, Rpairs, FArgs ),
     compound( FCall, Fun, FArgs ), % SWI-7 specific if FList is []
     memberchk( fcall(FCall), Opts ),
     ( memberchk(rvar(Rvar),Opts) ->
	Callable = (Rvar <- FCall)
	;
	Callable = (<- FCall)
     ),
     memberchk( call(CallBool), Opts ),
     call_r_function( CallBool, Callable, Opts ).

%%% end of interface predicates

eq_pair( =(_,_) ).

%% options_equals_pairs( +Opts, -Rpairs ).
%
% Extract the first K=V pair for all K=_ in Opts.
%
% The rationale is that these pairs are present in a list of
% usual options.
%
% Making them stick out by using =/2 notation helps distinguish them.
% Requiring only the first means that Opts can include default values.
%
%==
% ?- options_equals_pairs( [k=1,be(not),l=3,k=a], Rpairs ).
% Rpairs = [k=1, l=3].
% ?- options_equals_pairs( [k=1,be(not),l=+a,k=a], Rpairs ).
% Rpairs = [k=1, l=+a].
%==
%
options_equals_pairs( Opts, Rpairs ) :-
     options_equals_pairs( Opts, [], Rpairs ).

options_equals_pairs( [], _SeenKs, [] ).
options_equals_pairs( [O|Os], SeenKs, Rpairs ) :-
     ( O = (K=+_V) ; O = (K=_V) ),
     !,
     ( memberchk(K,SeenKs) ->
	NextSKs = SeenKs,
	Rpairs = Tpairs
	;
	NextSKs = [K|SeenKs],
	Rpairs = [O|Tpairs]
     ),
     options_equals_pairs( Os, NextSKs, Tpairs ).
options_equals_pairs( [_O|Os], SeenKs, Rpairs ) :-
     options_equals_pairs( Os, SeenKs, Rpairs ).

call_r_function( false, _Callable, _Opts ) :- !.
call_r_function( _True, Callable, Opts ) :-
     memberchk( outputs(OutS), Opts ),
     to_list( OutS, Outs ),
     memberchk( stem(Stem), Opts ),
     maplist( r_call_output(Callable,Stem,Opts), Outs ).

r_call_output( Call, Stem, Opts, Out ) :-
     arity( Out, Ofun, _ ),
     ( Ofun == x11 ->
		arity( Pfx, Ofun, 0 )  % SWI-specific
		;
		file_name_extension( Stem, Ofun, File ),
		Pfx =.. [Ofun,+File]
     ),
     arg_append( Out, [], OutComp ), % converts to compound as a side-effect
     % term_compound( Out, OutComp ),
     arg_append( Pfx, OutComp, OutCall ),
     debug( real, 'Output call: ~w', (<- OutCall) ),
     ( Ofun == false ->
	true
	;
	<- OutCall
     ),
     debug( real, 'R call: ~w', (<- Call) ),
     call( Call ),
     ( memberchk(post_call(Post),Opts) ->
	debug( real, 'Post call: ~w', [Post] ),
	call( Post )
	;
	debug( real, 'No post call in: ~w', [Opts] )
     ),
     r_call_ouput_dev_off( Ofun ).

r_call_ouput_dev_off( false ) :- !.
r_call_ouput_dev_off( x11 ) :- !.
r_call_ouput_dev_off( _ ) :- r_devoff.

r_start_auto :-
     % current_predicate( prefs:start_r_auto/1 ),
     % prefs:start_r_auto( false ),
     current_prolog_flag( real_start, false ),
     !.
r_start_auto :-
     r_start.

r_send( R ) :-
     % send_r_codes( Rcodes ) :-
     atom_codes( R, Rcodes ), % fixme, make send_r_command/1 to understand atoms
     debug( real, 'Sending to R: ~s', [Rcodes] ),
     send_r_command( Rcodes ).

assignment(PlDataIn, Rvar) :-
     % atom( Rvar ),
     rvar_identifier( Rvar, Rvar, _ ),
     compound( PlDataIn, c, _Arity ),
     % functor( PlDataIn, c, _Arity ),
     send_c_vector(PlDataIn, Rvar), !,
     debug( real, 'Assigned c vector to R variable ~a.', [Rvar] ).

assignment(PlDataIn, Rvar) :-
     % atom( Rvar ),
          % we would like to use rvar_identifier here, instead of atom/1
          % but a$b <- 3 does not work with set_r_variable/2.
     rvar_identifier( Rvar, Rvar, _ ),
     pl_data( PlDataIn, PlData ),
     !,
     % term_to_atom( RvarIn, RvarAtom ),
     set_r_variable(Rvar, PlData),
     debug( real, 'Assigned Prolog data to R variable ~a.', [Rvar] ).

assignment( Rexpr, Rvar ) :-
     rvar_identifier( Rvar, _Rvar, RAssgn ),
     rexpr( '<-'(-RAssgn,Rexpr), TmpRs, R ),
     !,
     r_send( R ),
     maplist( r_remove, TmpRs ).

pl_data( PlData, PlData ) :-
     ( number(PlData); PlData=[_|_]; boolean_atom(PlData); PlData = @(_) ).
/*
pl_data( PlDataIn, PlData ) :-
     PlDataIn =.. [c|PlData].
*/

/** rvar_identifier( Rterm, Rvar, Rcodes ).

True if Rterm is an access term for an R variable Rvar and Rcodes
are the codes corresponding to Rterm. Note that it is not the
case that term_to_codes( Rterm, Rcodes ) holds. Rterm might contain code lists
that are contextually interpreted by R as slots or list item labels.
Or, Rterm might contain indices that we translate.

*/

rvar_identifier( Rt, Rv, Rc ) :-
     rvar_identifier_1( Rt, Rv, Ra ),
     !,
     % is_r_variable( Rv ),
     atom_codes( Ra, Rc ).

rvar_identifier_1( Rvar, Rvar, Rvar ) :-
     atom( Rvar ),
     ( catch(term_to_atom(Atom,Rvar),_,fail) ),
     Atom == Rvar.
/*
rvar_identifier_1( A..B, Atom, Atom ) :-
     atom(B),
     rvar_identifier_1( A, Aatom, _ ),
     atomic_list_concat( [Aatom,'.',B], Atom ).
     */
rvar_identifier_1( A$B, Rv, C ) :-
     rname_atom( B, Batom ),
     rvar_identifier_1( A, Rv, Aatom ),
     % term_to_atom( Aatom$Batom, C ).
     atomic_list_concat( [Aatom,'$',Batom], C ).
rvar_identifier_1( A@B, Rv, C ) :-
     rname_atom( B, Batom ),
     rvar_identifier_1( A, Rv, Aatom ),
     atomic_list_concat( [Aatom,'@',Batom], C ).
rvar_identifier_1( []([[B]],A), Rv, C ) :-
     rvar_identifier_1( A, Rv, Aatom ),
     rexpr( B, [], Batom ),
     atomic_list_concat( [Aatom,'[[',Batom,']]'], C ).
rvar_identifier_1( A^[[B]], Rv, C ) :-
     rvar_identifier_1( A, Rv, Aatom ),
     rexpr( B, [], Batom ),
     atomic_list_concat( [Aatom,'[[',Batom,']]'], C ).
rvar_identifier_1( [](B,A), A, C ) :-
     rindices( B, Batom ),
     % atom_codes( Batom, BCs ),
     atom_concat( A, Batom, C ).
rvar_identifier_1( A^B, A, C ) :-
     atom( A ),
     is_list( B ),
	rindices( B, Batom ),
     atom_concat( A, Batom, C ).

/** rexpr(V,_,_).

     Generate (or parse) an R expression as codes from/to a Prolog term.
*/
rexpr( V, [], '' ) :-
     var(V),
     !,
     throw(error(instantiation_error,r_interface)).
rexpr( Numb, [], Expr ) :-
     number(Numb),
     !,
     atom_number( Expr, Numb ).
rexpr( Atom, [], Atom ) :-
     atom(Atom), !.
rexpr( String, [], Atom ) :-
     current_predicate(string/1),
     string(String),
     !,
     rexpr_string( String, Atom ).
rexpr( +ToString, [], Atom ) :-
     !,
     rexpr_string( ToString, Atom ).
rexpr( -ToAtom, [], Atom ) :-
     stringable_atom( ToAtom, Atom ),
	!.
rexpr( =+(A,B), [], Atom ) :-
     !,
     rexpr( (A = +B), [], Atom ).
rexpr( Array, TmpRs, TmpV ) :-
     % Array = [_|_],
	is_list(Array),
	( Array == [] ->
		TmpV = 'c()',  % NULL
		TmpRs = []
		;
	array_to_c( Array, TmpV, TmpV ),
	TmpRs = [TmpV]
	),
	!.
% Only for SWI 6 and Yap ?
rexpr( Term, [], Atom ) :-
     compound( Term, '()', [Fname] ),
     !,
     atomic_list_concat( [Fname,'()'], Atom ).
% Allows   ls(.)  as an alternative writing of ls()
% fixme: explore doing compound/3 once and the using the functor to differantiate ...
rexpr( Term, [], Atom ) :-
     compound( Term, Name, ['.'] ),
     !,
     atomic_list_concat( [Name,'()'], Atom ).
% fixme, 15.04.08: not sure what this is:
rexpr( AKey, TmpRs, Atom ) :-
     compound(AKey,[], [[[Key]], A]),
     !,
     rexpr( A, Atmps, Aatm ),
     rexpr( Key, Ktmps, Katm ),
     atomic_list_concat( [Aatm,'[[',Katm,']]'], Atom ),
     append( Atmps, Ktmps , TmpRs ).

rexpr( A^[[Key]], TmpRs, Atom ) :-
     !,
     rexpr( A, Atmps, Aatm ),
     rexpr( Key, Ktmps, Katm ),
     atomic_list_concat( [Aatm,'[[',Katm,']]'], Atom ),
     append( Atmps, Ktmps, TmpRs ).
% fixme, 15.04.08: old syntax ?
rexpr( AList, TmpRs, Atom ) :-
     compound( AList, [], [List,A] ),
     !,
     rexpr( A, TmpRs, Aatm ),
     rindices( List, Latm  ),
     atomic_list_concat( [Aatm,Latm], Atom ).
rexpr( A^List, TmpRs, Atom ) :-
     is_list(List),
     !,
     rexpr( A, TmpRs, Aatm ),
     % rexpr_unquoted(A, TmpRs),
     rindices( List, Latm ),
     atomic_list_concat( [Aatm,Latm], Atom ).
rexpr( A$B, TmpRs, Atom ) :-
     !,
     rexpr( A, TmpRs, Aatm ),
	( atomic(B) ->
	rname( B, Batm ),
	atomic_list_concat( [Aatm,'$',Batm], Atom )
		;
		compound(B,[],[Args,Index]),
		atomic_list_concat( [Aatm,Index], '$', Left ),
		NewExpr =.. [[],Args,Left],
		rexpr( NewExpr, TmpRs, Atom )
	).
rexpr( A@B, TmpRs, Atom ) :-
     !,
     rexpr( A, TmpRs, Aatm ),
     rname( B, Batm ),
     atomic_list_concat( [Aatm,'@',Batm], Atom ).
rexpr((A :- B), TmpRs, Atom ) :- % fixme: test this
     !,
     rexpr( A, Atmps, Aatm ),
     rexpr( B, Btmps, Batm ),
     atomic_list_concat( [Aatm,' ',Batm], Atom ),
     append( Atmps, Btmps, TmpRs ).
rexpr( Term, TmpRs, Atom ) :-
     arity( Term, NaIn, 2 ),
     binary( NaIn, Na ),
     % atom_codes( Na, NaS ),
     arg( 1, Term, A ),
     arg( 2, Term, B ),
     !,
     % fixme: we need something better in the following line (nicos)
     left( Na, NaL ),
     rexpr( A, Atmps, Aatm ),
     % " ", NaS, " ",
     rexpr( B, Btmps, Batm ),
     right( Na, NaR ),
     atomic_list_concat( [NaL,Aatm,' ',Na,' ',Batm,NaR], Atom ),
     append( Atmps, Btmps, TmpRs ).

rexpr( Term, TmpRs, Atom ) :-
     compound( Term, F, Args ),
     % Term =.. [F|Args], NA 15.5.7: this is superflous, and in the case of
	% swi7's x11(), wrong. Maybe added by SA ?
     F \== '.',
     !,
     stringable_atom( F, Fatm ),
     rexprs(Args, true, F, TmpRs, InnerList ),
     atomic_list_concat( InnerList, ',', Inner ),
     atomic_list_concat( [Fatm,'(',Inner,')'], Atom ).

rexpr_string( ToString, Atom ) :-
     stringable_atom( ToString, InnerAtom ),
     atomic_list_concat( ['"',InnerAtom,'"'], Atom ).

stringable_atom( String, Atom ) :-
     current_predicate(string/1),
     string( String ),
     !,
     atom_string( Atom, String ).
stringable_atom( String, Atom ) :-
     atom( String ),
     !,
     Atom = String.
stringable_atom( Codes, Atom ) :-
     is_list( Codes ),
     !,
     atom_codes( Atom, Codes ).

left( Na, Left ) :-
     no_brace( Na ),
     !,
     Left = ''.
left( _Na, '(' ).

right( Na, Right ) :-
     no_brace( Na ),
     !,
     Right = ''.
right( _Na, ')' ).

no_brace(<-).
no_brace(=).
no_brace(+).

rexprs( [], _, _, [], [] ).
rexprs([Arg|Args], _Fin, Func, TmpRs, [Aatm|Argsatms] ) :-
     % ( Fin==true -> Sep='' ; Sep= ' ,' ),
     % ( Args == [] -> Sep =
     rexpr( Arg, Atmps, Aatm ),
     rexprs(Args, false, Func, Argstmps, Argsatms ),
     append( Atmps, Argstmps, TmpRs ).
     % atomic_list_concat( [Aatm,Argsatm], Sep, Atom ).

rindices( List, Atom ) :-
     rindex( List, Inner ),
     atomic_list_concat( ['[',Inner,']'], Atom ).

rindex( [], '' ).
rindex( [H|T], Atom ) :-
     rindex_element( H, Hatm ),
     rindex_comma( T, Comma ),
     rindex( T, TAtm ),
     atomic_list_concat( [Hatm,Comma,TAtm], Atom ).

rindex_element( *, '' ).
rindex_element( List, Atom ) :-
     is_list(List),
     !,
     rindex( List, Inner ),
     atomic_list_concat( ['c(',Inner,')'], Atom ).
rindex_element( +ToString, Atom ) :-
     !,
     rexpr_string( ToString, Atom ).
rindex_element( -El, Atom ) :-
     rindex_element( El, ElAtm ),
     atomic_list_concat( ['-', ElAtm], Atom ).
rindex_element( ElL:ElR, Atom ) :-
     rindex_element( ElL, Latm ),
     rindex_element( ElR, Ratm ),
     atomic_list_concat( [Latm,':',Ratm], Atom ).
rindex_element( CExp, Atom ) :-
     CExp =.. [c|Cs], !,
     rindex( Cs, Inner ),
     atomic_list_concat( [c,'(',Inner,')'], Atom ).
rindex_element( Term, Atom ) :-
	compound(Term),
	!,
	% fixme: 15.11.04 make sure [] below is steadfast
	rexpr( Term, [], Atom ).
rindex_element( Oth, Atom ) :-
     (integer(Oth);atom(Oth)),
     !,
     write_to_chars(Oth,Codes),
     atom_codes( Atom, Codes ).
rindex_element( CExp, _ ) :-
     throw(cannot_process_index(CExp)).

rindex_comma( [], '' ) :- !.
rindex_comma( _, ',' ).

/* obsolete ?
%% codes_string(Codes,Quoted).
% check a list is full of (utf ?) codes
% while replacing any " with \" to produce Quoted from Ascii
%
codes_string([],[]).
codes_string(.(C,Cs),Q) :-
     integer(C),
     % <=nicos.  char_type(C,ascii),
        % <=nicos.   \+ char_type(C,cntrl),
     char_my_utf8(C),
     sew_code( C, Q, T ),
     codes_string(Cs,T).

char_my_utf8( C ) :-
     char_type(C,graph),
     !.
char_my_utf8( C ) :-
     char_type(C,white).

%% ascii_code_sew( C, Q, T ).
%  Sew C or its quoted form on list Q with its tail returned in T.
%
sew_code( 34, [0'\\,0'"|T], T ) :- !.
sew_code( C, [C|T], T ).
*/

%% rname( +Name ).
%
% first cut in supporting places where R is expecting "names or string constants"
% as in the RHS of $ and @
%
rname( ToAtom, Atom ) :-
     stringable_atom( ToAtom, Atom ).

%% rname_atom( Rname, Atom ).
%
%  Holds for atomic Atom a map of Rname.
%  If Rname is a list is assumed to be a list of codes that is
%  atom_code(/2)d to Atom.
%
rname_atom( Rname, Atom ) :-
     ( atomic(Rname) ->
          Atom = Rname
          ;
          atom_codes( Atom, Rname )
     ).

check_quoted(true, _) --> !, "TRUE".
check_quoted(false, _) --> !, "FALSE".
check_quoted(A, _) --> { is_r_variable(A) }, !,
     { format(codes(Codes), '~a', [A]) },
     Codes.
check_quoted(A, _) -->
     { format(codes(Codes), '"~a"', [A]) },
     Codes.

add_number(El) -->
     { number_codes(El, Codes) },
     Codes.

% i am sure there is something missing here, else Rv
% is just used twice
array_to_c( Array, Rv, Rv ) :-
     fresh_r_variable( Rv ),
     set_r_variable( Rv, Array ).

fresh_r_variable(Plv) :-
     between( 1, 10000, I ),
     atomic_list_concat([pl,v,I], '_', Plv),
     \+ r_is_var(Plv),
     !.

% hmmmm
% originally this (binary/1) included a call to exist,
% this rightly fails on lm(speeds~exprs)
% we are converting this to an operators version and we might
% need to introduce a top-level version that checks for functions
binary( Plname, Rname ) :-
     current_op( _, Assoc, real:Plname ),
     binary_real_r( Plname, Rname ),
     once( binary_op_associativity( Assoc ) ).
     % atomic_list_concat( [exists,'("',Rname,'",mode="function")'],  Atom ),
     % atom_codes( Atom, Rcodes ),
     % rexpr_to_pl_term( Rcodes, Rbool ),
     % Rbool == true.

binary_real_r( Plname, Rname ) :-
     binary_real_op( Plname, Rname ),
     !.
binary_real_r( OpName, OpName ).

%% binary_real_op( +Plname, -Rname ).
%
% Rname is R's operator name for Plname. We only to define cases where Plname \== Rname.
%
binary_real_op(  @*@, '%*%' ).
binary_real_op(  @^@, '%o%' ).
binary_real_op(  @~@, '%in%' ).
binary_real_op(  //, '%/%' ).
binary_real_op( mod, '%%'  ).
binary_real_op( \= , '!='  ).
% binary_real_op( =<, <= ).
	% the alternative is to define '!=' but in usage the `` have to be included
binary_real_op( ; , '|'  ).
binary_real_op( :: , '||'  ).

binary_op_associativity( yfx ).
binary_op_associativity( xfy ).
binary_op_associativity( xfx ).

boolean_atom( true ).
boolean_atom( false ).

% Only on SWI, bug Vitor for at_halt/1.
r_halt :-
     r_started,
     r_devoff_all,
     stop_r,
     !.
r_halt.

% try to work with SWI v7's extensions on compounds
compound( Term, Name, Args ) :-
     current_predicate( compound_name_arguments/3 ),
     !,
     once( (compound(Term) ; (ground(Name),is_list(Args))) ),
     % !,
     compound_name_arguments( Term, Name, Args ).
compound( Term, Name, Args ) :-
     once( (compound(Term) ; (ground(Name),ground(Args))) ),
     Term =.. [Name,Args].

arity( Term, Name, Arity ) :-
     current_predicate( compound_name_arity/3 ),
     \+ atomic( Term ),
     !,
     compound_name_arity( Term, Name, Arity ).
arity( Term, Name, Arity ) :-
     functor( Term, Name, Arity ).

/* @version  0.3 2015/01/12, allow Term to be an atom
   */

arg_append( Term, AppList, New ) :-
     is_list( AppList ),
     !,
     ( compound(Term,Tname,TArgs) ->
	true
	;
	atom(Term), % fixme: atomic?
	Tname = Term,
	TArgs = []
     ),
     % Term =.. [Tname|TArgs],
     append( TArgs, AppList, NArgs ),
     compound( New, Tname, NArgs ).
     % New =.. [Tname|NArgs].
arg_append( Term, AppTerm, New ) :-
     compound( AppTerm ),
     !,
     % AppTerm =.. [_ATname|ATArgs],
     % Term =.. [Tname|TArgs],
     compound( AppTerm, _ATname, ATArgs ),
     compound( Term, Tname, TArgs ),
     append( TArgs, ATArgs, NArgs ),
     % New =.. [Tname|NArgs].
     compound( New, Tname, NArgs ).
arg_append( Term, AppAtomic, New ) :-
     atomic( AppAtomic ),
     % Term =.. [Tname|TArgs],
     compound( Term, Tname, TArgs ),
     append( TArgs, [AppAtomic], NArgs ),
     % New =.. [Tname|NArgs].
     compound( New, Tname, NArgs ).

swipl_wins_warn :-
     current_prolog_flag(hwnd,_), % true iff ran via swipl-win.exe
	\+ current_prolog_flag( real_wins_warn, false ),
     !,
     L = "    library(real) notice: ",
     A = "         There is a known issue with swipl-win.exe.",
     B = "         R's I/O streams cannot be connected to those of Prolog.",
     C = "         So for instance, <- print(x) does not print x to the terminal.",
     D = "         All other functionalities are fine.",
     E = "         To circumvent use things like X <- x, write( x ).",
     F = "         If you need printing on console from R, you can start SWI via swipl.exe",
     G = "         To avoid seeing this message ?- set_prolog_flag(real_wins_warn,false). before loading Real.",
     Lines = [nl,nl,L,nl,nl,A,nl,B,nl,C,nl,D,nl,E,nl,F,nl,G,nl,nl],
     print_message_lines(current_output, '', Lines ).
swipl_wins_warn.

real_thread_self( Self ) :-
     current_predicate( thread_self/1 ),
     thread_self( Self ).

% error handling
:- multifile prolog:message//1.

prolog:message(unhandled_exception(real_error(Message))) -->
     { debug( real, 'Unhandled ~p', Message ) },
     message(Message).

prolog:message(real_error(Message)) -->
     { debug( real, 'Real error ~p', Message ) },
     message(Message).

message( stop_r_is_buggy ) -->
     ['Currently r_end/0 has no effect as the recommended C code does not work.\nYour link to the R library is still alive'].
message( r_already_started ) -->
     ['R has already been started.'].
message( server_alread_running(Thread) ) -->
     ['R server thread already assigned as ~w'-[Thread] ].
message( no_server_thread ) -->
     ['r_serve/0 called with no designated R server thread' ].
message( server_thread_mismatch(Me,Server) ) -->
     ['r_serve/0 called in thread ~w, but designated server thread is ~w'-[Me,Server]].
message( correspondence ) -->
     ['R was unable to digest your statement, either syntax or existance error.' - [] ].
message( r_root ) -->
     ['Real was unable to find the R root directory. \n If you have installed R from sources set $R_HOME to point to $PREFIX/lib/R.\n You should also make sure libR.so is in a directory appearing in $LD_LIBRARY_PATH' - [] ].
message( thread(G,real_error(Exc)) ) -->
     % ( Ball = Real -> true; throw(real_error(thread(Real,Ball))) ).
     { debug( real, 'Exception ~p', Exc ) },
     message( Exc ),
     ['\nR above was caught from thread execution while invoking ~p' - [G] ].
% error(existence_error(r_variable,x),context(real:robj_to_pl_term/2,_G395)
% message( thread(G,error(existence_error(r_variable,X),_,_)) ) -->
message( thread(G,error(Error,Context)) ) -->
     % ( Ball = Real -> true; throw(real_error(thread(Real,Ball))) ).
     { debug( real, 'Attempt to print error/2 ball ~p', error(Error,Context) ) },
     { print_message( error, error(Error,Context)  ) },
     ['Above error was caught from thread execution while invoking ~p' - [G] ].
message( thread(G,Exc) ) -->
     { debug(real,'In with ~p',Exc) },
     ['R thread was unable to digest your statement ~p, and caught exception: ~p.' - [G,Exc] ].
message( r_new_exists(X) ) -->
     ['First argument of <<- exists as R variable: ~w.' - [X] ].
message( r_new_var(X) ) -->
     ['First argument of <<- is not an atom: ~w.' - [X] ].
message( r_new_inconsistent(X) ) -->  % we should never get to this really
     ['First argument of <<- is weird: ~w.' - [X] ].

%% to_list( +Term, -Listed ).
%
% Wrap Term into a list of it not one already.
% For converting to lists see term_to_list/3.
%
%==
% ?- to_list( atom, List ).
% List = [atom].
%==
to_list( Either, List ) :-
     ( (var(Either);(Either\=[_H|_T],Either\==[]) ) ->
	List = [Either]
	;
	List = Either
     ).

%%	expand_dotted_name(+TermIn, -TermOut) is det.
%
%	Translate Atom1.Atom2 and Atom.Compound   into 'Atom1.Atom2' and
%	'Atom1.Name'(Args).
%
%    JW: July, 2016.
%
expand_dotted_name(TermIn, TermOut) :-
	compound(TermIn), !,
	(   join_dot(TermIn, Out)
	->  TermOut = Out
	;   contains_dot(TermIn)
	->  compound_name_arguments(TermIn, Name, ArgsIn),
	    maplist(expand_dotted_name, ArgsIn, ArgsOut),
	    compound_name_arguments(TermOut, Name, ArgsOut)
	;   TermOut = TermIn
	).
expand_dotted_name(Term, Term).

join_dot(In, Out) :-
	compound_name_arguments(In, '.', [A,B]),
	atom(A),
	(   atom(B)
	->  atomic_list_concat([A,'.',B], Out)
	;   compound(B)
	->  compound_name_arguments(B, Name, Args),
	    atomic_list_concat([A,'.',Name], Name2),
	    compound_name_arguments(Out, Name2, Args)
	;   Out = In
	).

contains_dot(Term) :-
	compound(Term),
	(   compound_name_arity(Term, '.', 2)
	->  true
	;   arg(_, Term, Arg),
	    contains_dot(Arg)
	->  true
	).

% JW July 2016
user:goal_expansion(In, Out) :-
	contains_dot(In), !,
	expand_dotted_name(In, Out).
% JW --end

user:portray( r(R) ) :-
     format('<- ~w', [R] ).
user:portray( r(L,R) ) :-
     format('~w <- ~w', [L,R]).

:- ( current_prolog_flag(version_data,swi(_,_,_,_)) -> at_halt(r_halt); true ).
:- initialization(r_start_auto, now).
