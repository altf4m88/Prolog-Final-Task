:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).

% Start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Declare a handler for the root directory
:- http_handler(root(.), form_with_joke_page, []).

% Function to fetch a random joke from the API
get_random_joke(Joke) :-
    URL = 'https://official-joke-api.appspot.com/random_joke',
    http_get(URL, JSONAtom, []),
    atom_json_dict(JSONAtom, Dict, []),
    Setup = Dict.get(setup),
    Punchline = Dict.get(punchline),
    atom_concat(Setup, " ", SetupSpace),
    atom_concat(SetupSpace, Punchline, Joke).

% Page that displays the static form along with the joke
form_with_joke_page(Request) :-
    get_random_joke(Joke),
    reply_html_page(
        title('Project Proposal Form with Joke'),
        [ \html_requires_style,
        h1('Project Proposal Form'),
          p('Here is a random joke for you (ngetes requestt API):'),
          p(Joke),
          h2('Form Details'),
          p('Kelompok: 8'),
          p('Nama anggota:'),
          p('1. Marhaensalenindo K'),
          p('2. M Fadhil Mauladhani'),
          p('3. Agung Prasetyo'),
          p('Usulan proyek tugas: Info Kurs Mata Uang'),
          p('Alasan: simple, tidak perlu setup DB dan informatif. Pemanfaatan API publik.')
        ]).

html_requires_style -->
    html(style('
        body { font-family: Arial, sans-serif; padding: 20px; line-height: 1.6; }
        h1 { color: #333; }
        p { margin: 10px 0; padding: 5px; background-color: #f4f4f4; border-radius: 5px; }
    ')).

% Start the server on port 8080 (or any other port you prefer)
:- initialization(server(8080)).