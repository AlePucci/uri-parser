%%%% -*- Mode: Prolog -*-
%%%% uri-parse.pl --

%%%% Alessandro Pucci 869177
%%%% Matias Aldo Ruiz 869139

%% The purpose of this project is to create a library
%% that can build structures, internally representing
%% URIs (Universal Resource Identifiers), starting from their
%% representations as strings.
%% Parsing requires analyzing and decomposing the input string
%% in sequence, character from character from left to right,
%% in order to build a proper structure that stores its
%% seven components.

%% If not specified, in every scheme, 80 is the default port.
%% An empty field, if the URI is still valid, is set to [].

%%% uri_parse/2
% The predicate is true when URIString is a valid "uri" and
% URI is the same string but broken down into its seven parts.
% URI = uri(Scheme, Userinfo, Host, Port, Path, Query and Fragment).
uri_parse(URIString, URI) :-
    string_chars(URIString, URIList),
    phrase(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment),
           URIList),
    flatten(Scheme, FScheme),
    adjust(AScheme, FScheme),
    flatten(Userinfo, FUserinfo),
    adjust(AUserinfo, FUserinfo),
    flatten(Host, FHost),
    adjust(AHost, FHost),
    flatten(Port, FPort),
    adjust_number(NPort, FPort),
    flatten(Path, FPath),
    adjust(APath, FPath),
    flatten(Query, FQuery),
    adjust(AQuery, FQuery),
    flatten(Fragment, FFragment),
    adjust(AFragment, FFragment),
    URI = uri(AScheme, AUserinfo, AHost, NPort, APath, AQuery, AFragment),
    !.

%%% uri_display/1
% Prints URI in the standard output, in a text format and closes
% the stream.
uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment),
                user_output).

%%% uri_display/2
% Prints URI in the provided stream (Out), in a text format and
% closes the stream.
uri_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment),
            Out) :-
    write(Out, 'Scheme   ==> '),
    write(Out, Scheme), nl(Out),
    write(Out, 'Userinfo ==> '),
    write(Out, Userinfo), nl(Out),
    write(Out, 'Host     ==> '),
    write(Out, Host), nl(Out),
    write(Out, 'Port     ==> '),
    write(Out, Port), nl(Out),
    write(Out, 'Path     ==> '),
    write(Out, Path), nl(Out),
    write(Out, 'Query    ==> '),
    write(Out, Query), nl(Out),
    write(Out, 'Fragment ==> '),
    write(Out, Fragment),
    close(Out).

%%% uri/7
% DCG for the special syntax schemes (zos excluded).
% The predicate is true when it is called with a valid
% URIstring (which has either "mailto", "news", "tel"
% or "fax" in the scheme part).
uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    scheme(mailto, Scheme), [':'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(mailto, Scheme), [':'], userinfo(Userinfo),
    { Host = "[]", Port = "80", Path = "[]", Query = "[]",
      Fragment = "[]" } |
    scheme(mailto, Scheme), [':'], !, userinfo(Userinfo), ['@'],
    host(Host),
    { Port = "80", Path = "[]", Query = "[]",
      Fragment = "[]" }  |
    scheme(news, Scheme), [':'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(news, Scheme), [':'], !, host(Host),
    { Userinfo = "[]", Port = "80", Path = "[]", Query = "[]",
      Fragment = "[]" } |
    scheme(tel, Scheme), [':'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(tel, Scheme), [':'], !, userinfo(Userinfo),
    { Host = "[]", Port = "80", Path = "[]", Query = "[]",
      Fragment = "[]" } |
    scheme(fax, Scheme), [':'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(fax, Scheme), [':'], !, userinfo(Userinfo),
    { Host = "[]", Port = "80", Path = "[]", Query = "[]",
      Fragment = "[]" }.

%%% uri/7
% DCG for the zos scheme, which describes data-sets names on
% IBM mainframes.
% The predicate is true when it is called with a valid
% URIstring (which has "zos" in the scheme part).
uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    scheme(zos, Scheme), [':'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(zos, Scheme), [':'], ['/'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], ['?'], query(Query),
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Fragment = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], ['#'], fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], ['?'], query(Query), ['#'],
    fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], path_zos(Path),
    { Userinfo = "[]", Host = "[]", Port = "80", Query = "[]",
      Fragment = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], path_zos(Path), ['?'],
    query(Query),
    { Userinfo = "[]", Host = "[]", Port = "80",
      Fragment = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], path_zos(Path), ['#'],
    fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80", Query = "[]" } |
    scheme(zos, Scheme), [':'], ['/'], path_zos(Path), ['?'],
    query(Query), ['#'], fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    { Path = "[]", Query = "[]", Fragment = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'],
    { Path = "[]", Query = "[]", Fragment = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'], path_zos(Path),
    { Query = "[]", Fragment = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'], ['?'], query(Query),
    { Path = "[]", Fragment = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'], ['#'], fragment(Fragment),
    { Path = "[]", Query = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'], ['?'], query(Query), ['#'], fragment(Fragment),
    { Path = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'], path_zos(Path), ['?'], query(Query),
    { Fragment = "[]" } |
    scheme(zos, Scheme), [':'], authority([Userinfo, Host, Port]),
    ['/'], path_zos(Path), ['#'], fragment(Fragment),
    { Query = "[]" } |
    scheme(zos, Scheme), [':'], !, authority([Userinfo, Host, Port]),
    ['/'], path_zos(Path), ['?'], query(Query), ['#'], fragment(Fragment).

%%% uri/7
% DCG for the classic scheme syntax.
% The predicate is true when it is called with a valid
% URIstring (which has the scheme part different from
% "mailto", "news", "tel", "fax" and "zos").
uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
    scheme(Scheme), [':'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(Scheme), [':'], ['/'],
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]", Fragment = "[]" } |
    scheme(Scheme), [':'], ['/'], ['?'], query(Query),
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Fragment = "[]" } |
    scheme(Scheme), [':'], ['/'], ['#'], fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]",
      Query = "[]" } |
    scheme(Scheme), [':'], ['/'], ['?'], query(Query), ['#'],
    fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80", Path = "[]" } |
    scheme(Scheme), [':'], ['/'], path(Path),
    { Userinfo = "[]", Host = "[]", Port = "80", Query = "[]",
      Fragment = "[]" } |
    scheme(Scheme), [':'], ['/'], path(Path), ['?'], query(Query),
    { Userinfo = "[]", Host = "[]", Port = "80", Fragment = "[]" } |
    scheme(Scheme), [':'], ['/'], path(Path), ['#'],
    fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80", Query = "[]" } |
    scheme(Scheme), [':'], ['/'], path(Path), ['?'], query(Query),
    ['#'], fragment(Fragment),
    { Userinfo = "[]", Host = "[]", Port = "80" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]),
    { Path = "[]", Query = "[]", Fragment = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    { Path = "[]", Query = "[]", Fragment = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    path(Path),
    { Query = "[]", Fragment = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    ['?'], query(Query),
    { Path = "[]", Fragment = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    ['#'], fragment(Fragment),
    { Path = "[]", Query = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    ['?'], query(Query), ['#'], fragment(Fragment),
    { Path = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    path(Path), ['?'], query(Query),
    { Fragment = "[]" } |
    scheme(Scheme), [':'], authority([Userinfo, Host, Port]), ['/'],
    path(Path), ['#'], fragment(Fragment),
    { Query = "[]" } |
    scheme(Scheme), [':'], !, authority([Userinfo, Host, Port]), ['/'],
    path(Path), ['?'], query(Query), ['#'], fragment(Fragment).

%% alpha('X') where X is a letter is used to include both uppercase
%% and lowercase versions of X, because URI is case insensitive.
scheme(mailto, Scheme) -->
    (alpha('m') | alpha('M')), (alpha('a') | alpha('A')),
    (alpha('i') | alpha('I')), (alpha('l') | alpha('L')),
    (alpha('t') | alpha('T')), (alpha('o') | alpha('O')),
    { Scheme = "mailto" }.
scheme(news, Scheme) -->
    (alpha('n') | alpha('N')), (alpha('e') | alpha('E')),
    (alpha('w') | alpha('W')), (alpha('s') | alpha('S')),
    { Scheme = "news" }.
scheme(tel, Scheme) -->
    (alpha('t') | alpha('T')), (alpha('e') | alpha('E')),
    (alpha('l') | alpha('L')),
    { Scheme = "tel" }.
scheme(fax, Scheme) -->
    (alpha('f') | alpha('F')), (alpha('a') | alpha('A')),
    (alpha('x') | alpha('X')),
    { Scheme = "fax" }.
scheme(zos, Scheme) -->
    (alpha('z') | alpha('Z')), (alpha('o') | alpha('O')),
    (alpha('s') | alpha('S')),
    { Scheme = "zos" }.
scheme(Identifier) --> identifier(Identifier).

authority([Userinfo, Host, Port]) -->
    double_slash, host(Host), { Userinfo = "[]", Port = "80" }.
authority([Userinfo, Host, Port]) -->
    double_slash, userinfo(Userinfo), ['@'], host(Host),
    { Port = "80" }.
authority([Userinfo, Host, Port]) -->
    double_slash, host(Host), [':'], port(Port),
    { Userinfo = "[]" }.
authority([Userinfo, Host, Port]) -->
    double_slash, userinfo(Userinfo), ['@'], host(Host), [':'],
    port(Port).

userinfo(Identifier) --> identifier(Identifier).

host(IP) --> ip_address(IP).
host(Identifier) --> identifier_host(Identifier).
host([Identifier, '.', Identifiers]) -->
    identifier_host(Identifier), ['.'], host(Identifiers).

port(Digit) --> digit(Digit).
port([Digit, Digits]) --> digit(Digit), port(Digits).

%% IP(v4) addresses must be in the following form:
%% NNN.NNN.NNN.NNN (with 0 <= NNN <= 255)
ip_address([IP1, '.', IP2, '.', IP3, '.', IP4]) -->
    octet(IP1), ['.'], octet(IP2), ['.'], octet(IP3), ['.'], octet(IP4).

path(Identifier) --> identifier(Identifier).
path([Identifier, '/', Identifiers]) -->
    identifier(Identifier), ['/'], path(Identifiers).

path_zos(Char) --> init(Char).
path_zos([Char | Identifiers]) --> init(Char), id44(1, Identifiers).
path_zos([Char1 | Char2]) --> init(Char1), ['('], init(Char2), [')'].
path_zos([Char1, ['('], [Char2 | Identifiers2], [')']]) -->
    init(Char1), ['('], init(Char2), id8(1, Identifiers2), [')'].
path_zos([[Char1 | Identifiers1], ['('], Char2, [')']]) -->
    init(Char1), id44(1, Identifiers1), ['('], init(Char2), [')'].
path_zos([[Char1 | Identifiers1], ['('],
	  [Char2 | Identifiers2], [')']]) -->
    init(Char1), id44(1, Identifiers1), ['('], init(Char2),
    id8(1, Identifiers2), [')'].

query(Char) --> char_3(Char).
query([Char | Chars]) --> char_3(Char), query(Chars).

fragment(Char) --> everything(Char).
fragment([Char | Chars]) --> everything(Char), fragment(Chars).

identifier(Char) --> char_1(Char).
identifier([Char | Chars]) --> char_1(Char), identifier(Chars).

identifier_host(Char) --> char_2(Char).
identifier_host([Char | Chars]) -->
    char_2(Char), identifier_host(Chars).

id44(_, Char) --> alphanum(Char).
id44(Len, [Char | Chars]) -->
    char_4(Char), { Len < 43 }, id44(Len + 1, Chars).

id8(_, Char) --> alphanum(Char).
id8(Len, [Char | Chars]) -->
    alphanum(Char), { Len < 7 }, id8(Len + 1, Chars).

%% path_zos identifiers must begin with a letter.
init(Char) --> alpha(Char).

octet(D) --> digit(D).
octet([D1, D2]) --> digit(D1), digit(D2).
octet(['0', D]) --> ['0'], digit(D).
octet(['0', D1, D2]) --> ['0'], digit(D1), digit(D2).
octet(['1', D1, D2]) --> ['1'], digit(D1), digit(D2).
octet(['2', '0', D]) --> ['2'], ['0'], digit(D).
octet(['2', '1', D]) --> ['2'], ['1'], digit(D).
octet(['2', '2', D]) --> ['2'], ['2'], digit(D).
octet(['2', '3', D]) --> ['2'], ['3'], digit(D).
octet(['2', '4', D]) --> ['2'], ['4'], digit(D).
octet(['2', '5', '0']) --> ['2'], ['5'], ['0'].
octet(['2', '5', '1']) --> ['2'], ['5'], ['1'].
octet(['2', '5', '2']) --> ['2'], ['5'], ['2'].
octet(['2', '5', '3']) --> ['2'], ['5'], ['3'].
octet(['2', '5', '4']) --> ['2'], ['5'], ['4'].
octet(['2', '5', '5']) --> ['2'], ['5'], ['5'].

%%% adjust/2
% This predicate is true when X is equal to ["[]"]
% and Y is equal to [].
% The predicate is useful to display the empty list
% without square brackets and double quotes.
adjust([], ["[]"]) :-
    !.

%%% adjust/2
% This predicate is true when X is equal to ["mailto"]
% and Y is equal to mailto.
adjust(mailto, ["mailto"]) :-
    !.

%%% adjust/2
% This predicate is true when X is equal to ["news"]
% and Y is equal to news.
adjust(news, ["news"]) :-
    !.

%%% adjust/2
% This predicate is true when X is equal to ["tel"]
% and Y is equal to tel.
adjust(tel, ["tel"]) :-
    !.

%%% adjust/2
% This predicate is true when X is equal to ["fax"]
% and Y is equal to fax.
adjust(fax, ["fax"]) :-
    !.

%%% adjust/2
% This predicate is true when X is equal to ["zos"]
% and Y is equal to zos.
adjust(zos, ["zos"]) :-
    !.

%%% adjust/2
% The predicate is true when X is different from
% empty list or a special scheme and Y is X converted into atom.
adjust(Y, X) :-
    atom_string(Y, X),
    !.

%%% adjust_number/2
% The predicate is true when X is different from
% ["80"] and Y is X converted into a number.
adjust_number(Y, X) :-
    X \= ["80"],
    !,
    number_string(Y, X).

%%% adjust_number/2
% The predicate is true when X is equal to "["80"]"
% and Y is equal to 80.
adjust_number(80, ["80"]) :-
    !.

%
% The following are all of the characters accepted in a URI,
% according to RFC (Request For Comment) 3986, section 2
% "Characters":
% https://datatracker.ietf.org/doc/html/rfc3986#section-2
% char_1 / char_2 / char_3 / char_4 are subsets of it,
% depending on the list of accepted characters in each part
% of an Universal Resource Identifier.
%

alphanum(Alpha) --> alpha(Alpha).
alphanum(Digit) --> digit(Digit).

alpha(Lower) --> lower_alpha(Lower).
alpha(Upper) --> upper_alpha(Upper).

unreserved(Alphanum) --> alphanum(Alphanum).
unreserved('-') --> ['-'].
unreserved('.') --> ['.'].
unreserved('_') --> ['_'].
unreserved('~') --> ['~'].

reserved(Gen_delims) --> gen_delims(Gen_delims).
reserved(Sub_delims) --> sub_delims(Sub_delims).

char_1(Unreserved) --> unreserved(Unreserved).
char_1(Sub_delims) --> sub_delims(Sub_delims).
char_1('[') --> ['['].
char_1(']') --> [']'].
char_1(Pct_encoded) --> pct_encoded(Pct_encoded).

char_2(Alphanum) --> alphanum(Alphanum).
char_2('-') --> ['-'].
char_2('_') --> ['_'].
char_2('~') --> ['~'].
char_2(Sub_delims) --> sub_delims(Sub_delims).
char_2('[') --> ['['].
char_2(']') --> [']'].
char_2(Pct_encoded) --> pct_encoded(Pct_encoded).

char_3(Unreserved) --> unreserved(Unreserved).
char_3(Sub_delims) --> sub_delims(Sub_delims).
char_3(':') --> [':'].
char_3('/') --> ['/'].
char_3('?') --> ['?'].
char_3('@') --> ['@'].
char_3('[') --> ['['].
char_3(']') --> [']'].
char_3(Pct_encoded) --> pct_encoded(Pct_encoded).

char_4(Alphanum) --> alphanum(Alphanum).
char_4('.') --> ['.'].

everything(Unreserved) --> unreserved(Unreserved).
everything(Reserved) --> reserved(Reserved).
everything(Pct_encoded) --> pct_encoded(Pct_encoded).

gen_delims(':') --> [':'].
gen_delims('/') --> ['/'].
gen_delims('?') --> ['?'].
gen_delims('#') --> ['#'].
gen_delims('[') --> ['['].
gen_delims(']') --> [']'].
gen_delims('@') --> ['@'].

sub_delims('!') --> ['!'].
sub_delims('$') --> ['$'].
sub_delims('&') --> ['&'].
sub_delims('\'') --> ['\''].
sub_delims('(') --> ['('].
sub_delims(')') --> [')'].
sub_delims('*') --> ['*'].
sub_delims('+') --> ['+'].
sub_delims(',') --> [','].
sub_delims(';') --> [';'].
sub_delims('=') --> ['='].

pct_encoded(['%', Hex1, Hex2]) -->
    ['%'], hex_digit(Hex1), hex_digit(Hex2).

hex_digit(Hex) --> digit(Hex).
hex_digit('a') --> ['a'].
hex_digit('b') --> ['b'].
hex_digit('c') --> ['c'].
hex_digit('d') --> ['d'].
hex_digit('e') --> ['e'].
hex_digit('f') --> ['f'].
hex_digit('A') --> ['A'].
hex_digit('B') --> ['B'].
hex_digit('C') --> ['C'].
hex_digit('D') --> ['D'].
hex_digit('E') --> ['E'].
hex_digit('F') --> ['F'].

lower_alpha('a') --> ['a'].
lower_alpha('b') --> ['b'].
lower_alpha('c') --> ['c'].
lower_alpha('d') --> ['d'].
lower_alpha('e') --> ['e'].
lower_alpha('f') --> ['f'].
lower_alpha('g') --> ['g'].
lower_alpha('h') --> ['h'].
lower_alpha('i') --> ['i'].
lower_alpha('j') --> ['j'].
lower_alpha('k') --> ['k'].
lower_alpha('l') --> ['l'].
lower_alpha('m') --> ['m'].
lower_alpha('n') --> ['n'].
lower_alpha('o') --> ['o'].
lower_alpha('p') --> ['p'].
lower_alpha('q') --> ['q'].
lower_alpha('r') --> ['r'].
lower_alpha('s') --> ['s'].
lower_alpha('t') --> ['t'].
lower_alpha('u') --> ['u'].
lower_alpha('v') --> ['v'].
lower_alpha('w') --> ['w'].
lower_alpha('x') --> ['x'].
lower_alpha('y') --> ['y'].
lower_alpha('z') --> ['z'].

upper_alpha('A') --> ['A'].
upper_alpha('B') --> ['B'].
upper_alpha('C') --> ['C'].
upper_alpha('D') --> ['D'].
upper_alpha('E') --> ['E'].
upper_alpha('F') --> ['F'].
upper_alpha('G') --> ['G'].
upper_alpha('H') --> ['H'].
upper_alpha('I') --> ['I'].
upper_alpha('J') --> ['J'].
upper_alpha('K') --> ['K'].
upper_alpha('L') --> ['L'].
upper_alpha('M') --> ['M'].
upper_alpha('N') --> ['N'].
upper_alpha('O') --> ['O'].
upper_alpha('P') --> ['P'].
upper_alpha('Q') --> ['Q'].
upper_alpha('R') --> ['R'].
upper_alpha('S') --> ['S'].
upper_alpha('T') --> ['T'].
upper_alpha('U') --> ['U'].
upper_alpha('V') --> ['V'].
upper_alpha('W') --> ['W'].
upper_alpha('X') --> ['X'].
upper_alpha('Y') --> ['Y'].
upper_alpha('Z') --> ['Z'].

digit('0') --> ['0'].
digit('1') --> ['1'].
digit('2') --> ['2'].
digit('3') --> ['3'].
digit('4') --> ['4'].
digit('5') --> ['5'].
digit('6') --> ['6'].
digit('7') --> ['7'].
digit('8') --> ['8'].
digit('9') --> ['9'].

double_slash --> ['/'], ['/'].

%%%% end of file -- uri-parse.pl
