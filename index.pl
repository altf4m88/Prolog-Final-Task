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

% Function to fetch exchange rate data safely with logging
get_exchange_rates(Rates) :-
    URL = 'https://api.frankfurter.app/2020-01-01..2020-01-31?from=EUR&to=USD',
    catch(http_get(URL, JSONAtom, []), Error, (print_message(error, Error), fail)),
    catch(atom_json_dict(JSONAtom, Dict, []), Error, (print_message(error, Error), fail)),
    (   is_dict(Dict.rates)
    ->  Rates = Dict.rates,
        format(user_error, 'Rates fetched: ~w~n', [Rates])  % Logging fetched rates
    ;   print_message(error, 'No rates data found'), fail).

% Convert rates dictionary to lists of dates and values with refined method
convert_rates_to_lists(RatesDict, Dates, Values) :-
    is_dict(RatesDict),  % Check if the RatesDict is a dictionary
    dict_pairs(RatesDict, _, Pairs),  % Get the key-value pairs from the dictionary
    pairs_keys_values(Pairs, Dates, ValueDicts),  % Separate the keys and values into separate lists
    maplist(extract_usd_value, ValueDicts, Values).  % Extract USD values from each dictionary

% Helper predicate to extract USD value from nested dictionary
extract_usd_value(Dict, Value) :-
    is_dict(Dict),  % Ensure it is a dictionary
    Value = Dict.USD.  % Extract the USD value

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
    get_exchange_rates(Rates),
    convert_rates_to_lists(Rates, Dates, Values),
    reply_html_page(
        title('Project Proposal Form with Joke and Chart'),
        [ \html_requires_chartjs,
          \html_requires_style,
          h1('Project Proposal Form'),
          p('Here is a random joke for you:'),
          p(Joke),
          \chart_canvas,
          \initialize_chart(Dates, Values)
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

initialize_chart(Dates, Values) -->
    {
        atom_json_term(DatesJSON, Dates, []),
        atom_json_term(ValuesJSON, Values, []),
        format(string(ChartJS), '
            var ctx = document.getElementById("myChart").getContext("2d");
            var myChart = new Chart(ctx, {
                type: "line",
                data: {
                    labels: ~w,
                    datasets: [{
                        label: "EUR to USD Exchange Rate",
                        data: ~w,
                        backgroundColor: "rgba(54, 162, 235, 0.2)",
                        borderColor: "rgba(54, 162, 235, 1)",
                        borderWidth: 1
                    }]
                },
                options: {
                    scales: {
                        y: {
                            beginAtZero: false
                        }
                    }
                }
            });
        ', [DatesJSON, ValuesJSON])
    },
    html(script([], ChartJS)).


% The server initialization remains the same
:- initialization(server(8080)).