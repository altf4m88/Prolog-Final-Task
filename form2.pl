:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).

% Start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Declare a handler for the root directory
:- http_handler(root(.), form, []).

:- http_handler(root(chart), chart, [method(post)]).

% Function to fetch exchange rate data safely with logging
get_exchange_rates(From, To, Amount, Period, Rates) :-
    get_date_range(Period, StartDate, EndDate),
    format(string(URL), 'https://api.frankfurter.app/~w..~w?amount=~w&from=~w&to=~w', [StartDate, EndDate, Amount, From, To]),
    catch(http_get(URL, JSONAtom, []), Error, (print_message(error, Error), fail)),
    catch(atom_json_dict(JSONAtom, Dict, []), Error, (print_message(error, Error), fail)),
    (   is_dict(Dict.rates)
    ->  Rates = Dict.rates,
        format(user_error, 'Rates fetched: ~w~n', [Rates])  % Logging fetched rates
    ;   print_message(error, 'No rates data found'), fail).

% Fetch latest USD exchange rates for the pie chart
get_usd_exchange_rates(ExchangeRates) :-
    URL = 'https://open.er-api.com/v6/latest/USD',
    catch(http_get(URL, JSONAtom, []), Error, (print_message(error, Error), fail)),
    catch(atom_json_dict(JSONAtom, Dict, []), Error, (print_message(error, Error), fail)),
    (   is_dict(Dict.rates)
    ->  ExchangeRates = Dict.rates,
        format(user_error, 'USD Rates fetched: ~w~n', [ExchangeRates])  % Logging fetched rates
    ;   print_message(error, 'No USD rates data found'), fail).

% Helper predicate to format dates as "YYYY-MM-DD"
format_date(Date, FormattedDate) :-
    format_time(atom(FormattedDate), '%Y-%m-%d', Date).

% Helper predicate to calculate date ranges based on the selected period
get_date_range(week, StartDate, EndDate) :-
    get_time(Now),
    StartTimestamp is Now - 604800,  % 7 days in seconds
    EndTimestamp is Now,
    format_date(StartTimestamp, StartDate),
    format_date(EndTimestamp, EndDate).
get_date_range(month, StartDate, EndDate) :-
    get_time(Now),
    StartTimestamp is Now - 2592000,  % 30 days in seconds
    EndTimestamp is Now,
    format_date(StartTimestamp, StartDate),
    format_date(EndTimestamp, EndDate).
get_date_range(year, StartDate, EndDate) :-
    get_time(Now),
    StartTimestamp is Now - 31536000,  % 365 days in seconds
    EndTimestamp is Now,
    format_date(StartTimestamp, StartDate),
    format_date(EndTimestamp, EndDate).
get_date_range(five_years, StartDate, EndDate) :-
    get_time(Now),
    StartTimestamp is Now - 157680000,  % 5 years in seconds
    EndTimestamp is Now,
    format_date(StartTimestamp, StartDate),
    format_date(EndTimestamp, EndDate).

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
    get_currencies(Currencies),
    make_currency_options(Currencies, Options1),
    make_currency_options(Currencies, Options2),
    reply_html_page(
        title('Form'),
        [ \html_requires_style,
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
                         [ 
                            form([action='/chart', method='POST', class('form')],
                            [ div(class='form-group',
                                    [ label([for='currency1'], 'From:'),
                                    select([class='form-control', id='currency1', name='from'], Options1)
                                    ]),
                            div(class='form-group',
                                [ label([for='amount'], 'Amount:'),
                                    input([type=number, class='form-control', id='amount', name='amount', value=1, min=0, step=0.01])
                                ]),
                            div(class='form-group',
                                [ label([for='currency2'], 'To:'),
                                    select([class='form-control', id='currency2', name='to'], Options2)
                                ]),
                            div(class='form-group',
                                [ label([for='period'], 'Period:'),
                                    select([class='form-control', id='period', name='period'],
                                        [ option([value=week], 'Weekly'),
                                          option([value=month], 'Monthly'),
                                          option([value=year], 'Yearly'),
                                          option([value=five_years], 'Every Five Years')
                                        ])
                                ]),
                            input([class='btn btn-primary btn-block', type='submit', value='Convert'])
                            ])
                         ])
                    )
                )
            )
        ]).

chart(Request) :-
    http_parameters(Request,
                    [ from(From, []),
                      to(To, []),
                      amount(Amount, [number]),
                      period(Period, [])
                    ]),
    catch(
        (   get_exchange_rates(From, To, Amount, Period, Rates),
            convert_rates_to_lists(Rates, Dates, Values),
            get_usd_exchange_rates(USDExchangeRates),
            convert_usd_rates_to_pie_data(USDExchangeRates, PieLabels, PieValues),
            reply_html_page(
                title('Chart'),
                [ \html_requires_chartjs,
                  \html_requires_style,
                  head([
                      title('Chart Page'),
                      meta([name(viewport), content('width=device-width, initial-scale=1')]), 
                      link([rel('stylesheet'), href('https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css')]),
                      \html_css
                  ]),
                  div(class('container'), [
                    h1('Final Project Prolog'),
                    p(['Currency Conversion: ', Amount, ' ', From, ' to ', To, ' (Period: ', Period, ')']),
                    div(class('main-info'), [
                        div(class('chart-container'), [
                            h2('Currency Chart'),
                            canvas([id('myChart'), width('400'), height('400')], [])
                        ]),
                        div(class('info-card'), [
                            div(class('info-card-content'), [
                                p(['strong(Currency:)', ' USD']),
                                p(['strong(Price:)', ' 75']),
                                p(['strong(Date:)', ' 2024-05-17'])
                            ])
                        ])
                    ]),
                    div(class('rate-currency'), [
                        div(class('info-card'), [
                            div(class('info-card-content'), [
                                p(['strong(Currency:)', ' USD']),
                                p(['strong(Price:)', ' 75']),
                                p(['strong(Date:)', ' 2024-05-17'])
                            ])
                        ]),
                        div(class('info-rate-currency'), [
                            h2('Info Rate Currency By USD'),
                            canvas([id('myChartPie'), width('400'), height('400')], [])
                        ])
                    ])
                  ]),
                  \initialize_chart(Dates, Values),
                  \initialize_pie_chart(PieLabels, PieValues)
                ])
        ),
        Error,
        (   print_message(error, Error),
            reply_html_page(
                title('Error'),
                [ p('An error occurred while processing your request.') ]
            )
        )
    ).

html_css -->
    html(style('
        body { font-family: Arial, sans-serif; }
        .container { width: 80%; margin: 0 auto; }
        .container h1 { text-align: center; }
        .main-info { display: flex; justify-content: space-between; margin: 20px; }
        .main-info .chart-container { width: 75%; margin: 20px; }
        .info-card { width: 25%; margin: 50px 0 20px 20px; border: 1px solid #ddd
        ; padding: 20px; 
                     border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                     background: linear-gradient(to bottom, #0681c4, #8410ffb1); color: white; }
        .info-card .info-card-content p { margin: 10px 0; }
        .rate-currency { display: flex; justify-content: space-between; margin: 20px; height: 700px; }
        .rate-currency .info-rate-currency { width: 50%; }'
    )).

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
        canvas { margin-top: 20px; }
    ')).

% Element for the chart canvas
chart_canvas -->
    html(canvas([id('myChart'), width('400'), height('400')], [])).

% Convert USD exchange rates dictionary to labels and values for the pie chart
convert_usd_rates_to_pie_data(RatesDict, Labels, Values) :-
    is_dict(RatesDict),  % Check if the RatesDict is a dictionary
    dict_pairs(RatesDict, _, Pairs),  % Get the key-value pairs from the dictionary
    % Sort the pairs by values in descending order and take the first 5 pairs
    sort(1, @>=, Pairs, SortedPairs),
    take(5, SortedPairs, SelectedPairs),
    pairs_keys_values(SelectedPairs, Labels, Values).  % Separate the keys and values into separate lists

% Predicate to take the first N elements from a list
take(0, _, []).
take(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Rest).

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
                        label: "Exchange Rate",
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

initialize_pie_chart(Labels, Values) -->
    {
        atom_json_term(LabelsJSON, Labels, []),
        atom_json_term(ValuesJSON, Values, []),
        format(string(PieChartJS), '
            console.log("Labels:", ~w);
            console.log("Values:", ~w);

            var ctxPie = document.getElementById("myChartPie").getContext("2d");
            var myChartPie = new Chart(ctxPie, {
                type: "pie",
                data: {
                    labels: ~w,
                    datasets: [{
                        label: "Exchange Rates",
                        data: ~w,
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
                    responsive: true,
                    maintainAspectRatio: false
                }
            });
            document.getElementById("myChartPie").style.height = "400px"; // Menetapkan tinggi tetap
        ', [LabelsJSON, ValuesJSON, LabelsJSON, ValuesJSON])
    },
    html(script([], PieChartJS)).




% The server initialization remains the same
:- initialization(server(8080)).
