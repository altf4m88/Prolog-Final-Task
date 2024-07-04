:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

% Memulai server
start_server :-
    http_server(http_dispatch, [port(1111)]).

% Server HTTP
:- http_handler('/', serve_home, []).
:- http_handler('/static/', serve_static, [prefix]).

% Predikat untuk melayani file index.html
serve_home(Request) :-
    http_reply_file('index.html', [], Request).

% Predikat untuk melayani file statis
serve_static(Request) :-
    http_reply_from_files('.', [], Request).

% Memulai server pada port 1111 saat inisialisasi
:- initialization(start_server).
