:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).

% Start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Declare a handler for the root directory
:- http_handler(root(.), form, []).

:- http_handler(root(submit_profil), submit_profil_handler, [method(post)]).

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

% Page that displays the static form along with the joke and a chart
form(_Request) :-
    get_exchange_rates(Rates),
    convert_rates_to_lists(Rates, Dates, Values),
    get_currencies(Currencies),
    make_currency_options(Currencies, Options1),
    make_currency_options(Currencies, Options2),
    reply_html_page(
        title('Project Proposal Form with Joke and Chart'),
        [ \html_requires_chartjs,
          \html_requires_style,
          head([
              title('Home Page'),
              meta([name(viewport), content('width=device-width, initial-scale=1')]), 
              link([rel('stylesheet'), href('https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css')])
          ]),
          h1([class='text-center'], 'Form Currency'),
          div(class='row',
             div(class='col-md-6 offset-md-3',
                 div(class='card',
                     div(class='card-body',
                         [ div(class='form-group',
                                [ label([for='currency1'], 'From:'),
                                  select([class='form-control', id='currency1'], Options1)
                                ]),
                           div(class='form-group',
                               [ label([for='amount'], 'Amount:'),
                                 input([type=number, class='form-control', id='amount', value=1, min=0, step=0.01])
                               ]),
                           div(class='form-group',
                               [ label([for='currency2'], 'To:'),
                                 select([class='form-control', id='currency2'], Options2)
                               ]),
                           button([class='btn btn-primary btn-block', onclick='convertCurrency()'], 'Convert')
                         ])
                    )
                )
            ),
          \chart_canvas,
          \initialize_chart(Dates, Values)
        ]).

% Predicate to fetch the list of currencies from the API
get_currencies(Currencies) :-
    URL = 'https://api.frankfurter.app/currencies',
    catch(http_get(URL, JSONAtom, []), Error, (print_message(error, Error), fail)),
    catch(atom_json_dict(JSONAtom, Currencies, []), Error, (print_message(error, Error), fail)).

% Predicate to create options from the list of currencies
make_currency_options(Currencies, Options) :-
    findall(option([value=Code], Name), (get_dict(Code, Currencies, Name)), Options).

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
