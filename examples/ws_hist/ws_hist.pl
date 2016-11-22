
:- use_module( library(http_unix_daemon_real)). % http_daemon/0 slightly patched.
:- use_module( library(http/thread_httpd) ).
:- use_module( library(http/http_session) ).
:- use_module( library(http/html_write) ).    % page/4, see also html/3
:- use_module( library(http/http_client) ).   % http_read_data/3
:- use_module( library(http/http_dispatch) ). % http_reply_file/3

:- http_handler( /, reply, [prefix] ).
% :- listen( http(post_server_start), r_thread_loop ).
:- listen( http(pre_server_start), ws_hist_pre ).

:- use_module( library(real) ).
:- <- suppressPackageStartupMessages( library( "ggplot2" ) ).

/* when included SWI server fails on its setup_signals/0 predicate
huped( hup ) :-
	thread_send_message( main, stop ).

:- on_signal( hup, _Old, huped ).
*/

% ws_hist.
%
% Run a simple web server demonstrating threaded use of the Real pack. It runs on SWI.
% The server takes up to 4 lists of numbers and plots them as dodged histogram onto a PNG file.
%
% You can execute this file locally as far as you have R with the ggplot2 package.
%
% To run the server from the command line simply load this file into SWI. 
%
% Then open your browser at http://localhost:7171
%
% To run the server as a linux init.d script see http://stoics.org.uk/~nicos/sware/real/ws.html
%
% @author nicos angelopoulos
% @version  0.1 2014/03/23
%
ws_hist :-                         % called by initialization/2
	current_prolog_flag( argv, Args ),
	( (member(Arg,Args),atom_concat('--port',_,Arg)) -> true; fail ),
	!,
	http_daemon.
ws_hist :-
	thread_create( delete_tmp_file, _, [alias(del_tmp),detached(true)] ),
	http_server( reply, [ timeout(600), port(7171) ] ),
	r_thread_loop.
	
ws_hist_pre :-			% connected to the http(pre...) listener
	thread_create( delete_tmp_file, _, [alias(del_tmp),detached(true)] ).

/*
repot( Term ) :-   % if things go pairshape, try debugging: 
	open( '/tmp/repot.txt', append, Out ),
	portray_clause( Out, Term ),
	close( Out ).
	*/

reply( Request ) :-
	memberchk( path(Path), Request ),
	path_reply( Path, '', Request ).

path_reply( '/', Pfx, _ ) :-
	debug( ws_hist, 'In root', [] ),
	reply_form( Form ),
	header( Pfx, Header ),
	footer( Footer ),
	flatten( [Header,Form,Footer], Body ),
	Head = title('Real in web services example'),
	phrase( page(Head,Body), HTML),
	format('Content-type: text/html~n~n'),
	print_html( HTML ).

path_reply( '/plot', _, Request ) :-
	http_read_data(Request, Data, []),
	( memberchk(session(Sess),Request) -> true; Sess = none_found ),
	alpha_num( Sess, SessTkn ),
	atomic_list_concat( [df,SessTkn], '_', Df ),
	data_ggplot_df( Data, Df ),
	data_labels( Data, Xlab, Ylab ),
		GG = ( ggplot( Df, aes(x='as.factor'(pos), y=pop, fill='as.factor'(list))) 
		       + geom_bar( position="dodge", stat="identity", width=0.5 ) 
			  + scale_fill_discrete( name  = "lists") 
			  + labs( x=Xlab ) + labs( y=Ylab )
			),
	debug( ws_hist, 'attempting to plot: ~w', [GG] ),
	debug( wb_hist, 'Session: ~w', [Sess] ),
	directory_file_path( '/tmp', Sess, TmpSess ),
	file_name_extension( TmpSess, png, PngF ),
	atom_concat( TmpSess, '_dummy', CairoSess ),
	file_name_extension( CairoSess, png, CairoF ),
	% write( user_error, 'pnging' ), nl( user_error ),
	<- png( filename=+CairoF ), % this is only a decoy, to avoid an x11() starting
 	ggplot <- GG,
     <- suppressMessages( ggsave( plot=GG, filename=+PngF, dpi=72 ) ),
	devoff,
	<- remove( Df ),
	!,
	( getenv('USER',User) -> true; User = none ),
	path_record( User, successful, Data, Request ),
	debug( wb_hist, 'Png file: ~w', PngF ),
	thread_send_message( del_tmp, del(CairoF) ),
	thread_send_message( del_tmp, del(PngF) ),
	http_reply_file( PngF, [unsafe(true)], [] ).
path_reply( '/plot', _, Request ) :-
	( getenv('USER',User) -> true; User = none ),
	path_record( User, badly_formed, [], Request ),
	Pfx = 'Something is wrong with your input, have another go.',
	path_reply( '/', Pfx, Request ).

reply_form( Form ) :-
	Form = [h4(real_example,style('color:darkgreen')),
		   form( [action(plot),method('Post')],
		     table( [ 
		             tr( [
					  td( input([type(text),name(val1),value('1,2,3')]) ),
					  td( input([type(text),name(val2),value('2,4')]) ),
					  td( input([type(text),name(val3),value('12,5,0,2')]) ),
					  td( input([type(text),name(val4),value('1')]) )
					  ]
				     ),
				   tr( [] ),
				   tr( [td(align(right),x_label),
				        td( input( [type(text),name(xlab),value(xlab)] ) )
					   ] ),
				   tr( [td(align(right),y_label),
				        td( input( [type(text),name(ylab),value('some text')] ) )
					   ] ),
				   tr( [] ),
				   tr( [ td(''), td(''), td(''), td(input([type(submit),value('create plot')])) ] ),
				   tr( [] )
		            ] )
			  )
	       ].

data_ggplot_df( Data, DFRv ) :-
	maplist( plot_list_term(Data), [1,2,3,4], PlistTermsPrv ), % fail on long input 
	include( non_empty_arg_2, PlistTermsPrv, PLts ),
	maplist( arg(2), PLts, PLs ), 
	maplist( length, PLs, PLlengths ),
	max_list( PLlengths, Max ),
	findall( Nths, (between(1,Max,N),
					findall(Nth,(member(pl(_,Pl),PLts),nth1(N,Pl,Nth)),Nths)
			     ),
					NthPlNest ),
	flatten( NthPlNest,  Pop ),
	debug( ws_hist, 'Populations : ~w', [Pop] ),

	findall( OT, (between(1,Max,N),
					findall(I,(member(pl(I,Pl),PLts),nth1(N,Pl,_Nth1)),OT)
				),
					OTnest ),
	flatten( OTnest, ListIds ),
	debug( ws_hist, 'List ids: ~w', [ListIds] ),

	findall( Poss, (between(1,Max,N),
					findall(N,(member(pl(I,Pl),PLts),nth1(N,Pl,_Nth2)),Poss)
				),
					PositsNest ),
	flatten( PositsNest, Posits ),
	debug( ws_hist, 'Posits : ~w', [Posits] ),
 	DFRv  <- 'data.frame'( pop=Pop, list=ListIds, pos=Posits ).

non_empty_arg_2( Term ) :-
	arg( 2, Term, Arg2 ),
	Arg2 \== [].

plot_list_term( Data, I, pl(I,VList) ) :-
	atomic_list_concat( [val,I], VLname ),
 	memberchk( VLname=VListAtom, Data ),
	atom_length( VListAtom, VListAtomLen ),
	VListAtomLen < 200, 
	atomic_list_concat( VListAtomsPrv, ',', VListAtom ),
	empty_list( VListAtomsPrv, VListAtoms ),
     maplist( atom_number, VListAtoms, VList ).
	
empty_list( [''], [] ) :- !.
empty_list( List, List ).

data_labels( Data, Xlab, Ylab ) :-
	memberchk( xlab=XlabA, Data ),
	memberchk( ylab=YlabA, Data ),
	atom_string( XlabA, Xlab ),
	atom_string( YlabA, Ylab ).

header( Pfx, Header ) :- 
	Std1 = 'Enter up to 4 lists of comma separated numbers (each list should be less than 200 characters long).',
	Std2 = 'This script will create the "dodged" histogram of the inputs and serve it as a PNG.',
	Header = [     p( ['Demo for using ',a(href('http://stoics.org.uk/~nicos/sware/real'),real),
	                    ' ', ' in web services.'] ),
	               p( [Pfx,br(''),Std1,br(''),Std2] )
		    ].

footer( Footer ) :-
	( gethostname('vps34663.ovh.net') -> 
		Extra = '(Be patient for the reply, this is a tiny server with a miniscule amount of memory.)'
                ; 
		Extra = ''
	),
	Footer = [
				p(''), br(''),p(Extra),br(''),
					
				h3('software used in this demo'),
			 	p(''),
						ul( [
						li( a(href('http://www.swi-prolog.org/'),'SWI Prolog') ),
						li( a(href('http://www.r-project.org/'),'R') ),
						li( a(href('http://ggplot2.org'),'ggplot2') ),
						li( a(href('http://stoics.org.uk/~nicos/sware/real'),'Real') )
						] ),
				p(''),
				p( ['The sources for this example can be found at: ',
					a(href('http://stoics.org.uk/~nicos/sware/real/web_srv_hist.pl'),'web_srv_hist.pl'),'.' ]
				 ) 
			].

delete_tmp_file :-
	thread_get_message( Mess ),
	delete_tmp_file( Mess ).

delete_tmp_file( halt ) :- !, thread_exit( true ).
delete_tmp_file( del(Tmp) ) :-
	sleep( 2 ), % wait till the file is served
	( atom_concat('/tmp',_,Tmp) -> catch(delete_file(Tmp),_,true) ; true ),
	delete_tmp_file.

alpha_num( Atom, Token ) :-
	atom_codes( Atom, Codes ), 
	include( alpha_num_code, Codes, Incl ),
	atom_codes( Token, Incl ).

alpha_num_code( Code ) :-
	0'0 =< Code, Code =< 0'9.
alpha_num_code( Code ) :-
	0'a =< Code, Code =< 0'z.
alpha_num_code( Code ) :-
	0'A =< Code, Code =< 0'Z.

path_record( nicos, Res, _Data, Request ) :-
	( memberchk( peer(Peer), Request ) -> true; Peer = no_peer_information ),
	File = '/srv/www/html/log/nicos/real/ws_hist/log.pl',
	catch( open(File,append,Out), _, fail ),
	!,
	get_time(TimeStamp),
	stamp_date_time(TimeStamp, DateTime, local),
	DateTime =.. [date,A,B,C,D,E|_Args],
	Date =.. [date,A,B,C,D,E],
	Term =.. [Res,Date,Peer],
	portray_clause( Out, Term ),
	close( Out ).
%  you can enable logging for your setup by writing a new clause here
path_record( _User, _Res, _Data, _Request ).

:- initialization( ws_hist, after_load ).
