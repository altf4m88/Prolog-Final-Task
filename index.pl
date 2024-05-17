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

% Page that displays the static form along with the joke and a chart
form_with_joke_page(Request) :-
    get_random_joke(Joke),
    reply_html_page(
        title('Project Proposal Form with Joke and Chart'),
        [ \html_requires_chartjs,  % Include Chart.js
          \html_requires_style,
          h1('Project Proposal Form'),
          p('Here is a random joke for you:'),
          p(Joke),
          h2('Form Details'),
          p('Kelompok: 8'),
          p('Nama anggota:'),
          p('1. Marhaensalenindo K'),
          p('2. M Fadhil Mauladhani'),
          p('3. Agung Prasetyo'),
          p('Usulan proyek tugas: Info Kurs Mata Uang'),
          p('Alasan: simple, tidak perlu setup DB dan informatif. Pemanfaatan API publik.'),
          \chart_canvas,  % Canvas for the chart
          \initialize_chart  % Initialize the chart with JavaScript
        ]).


html_requires_chartjs -->
    html(script([src('https://cdn.jsdelivr.net/npm/chart.js')], [])).

% Include custom styles
html_requires_style -->
    html(style('
        body { font-family: Arial, sans-serif; padding: 20px; line-height: 1.6; }
        h1, h2 { color: #333; }
        p { margin: 10px 0; padding: 5px; background-color: #f4f4f4; border-radius: 5px; }
        canvas { margin-top: 20px; }
    ')).

% Element for the chart canvas
chart_canvas -->
    html(canvas([id('myChart'), width('400'), height('400')], [])).

% Initialize the chart with JavaScript
initialize_chart -->
    html(script([], '
        var ctx = document.getElementById("myChart").getContext("2d");
        var myChart = new Chart(ctx, {
            type: "bar",
            data: {
                labels: ["Red", "Blue", "Yellow", "Green", "Purple", "Orange"],
                datasets: [{
                    label: "# of Votes",
                    data: [12, 19, 3, 5, 2, 3],
                    backgroundColor: [
                        "rgba(255, 99, 132, 0.2)",
                        "rgba(54, 162, 235, 0.2)",
                        "rgba(255, 206, 86, 0.2)",
                        "rgba(75, 192, 192, 0.2)",
                        "rgba(153, 102, 255, 0.2)",
                        "rgba(255, 159, 64, 0.2)"
                    ],
                    borderColor: [
                        "rgba(255, 99, 132, 1)",
                        "rgba(54, 162, 235, 1)",
                        "rgba(255, 206, 86, 1)",
                        "rgba(75, 192, 192, 1)",
                        "rgba(153, 102, 255, 1)",
                        "rgba(255, 159, 64, 1)"
                    ],
                    borderWidth: 1
                }]
            },
            options: {
                scales: {
                    y: {
                        beginAtZero: true
                    }
                }
            }
        });
    ')).

% The server initialization remains the same
:- initialization(server(8080)).