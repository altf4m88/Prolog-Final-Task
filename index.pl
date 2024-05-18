:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

% Start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Declare a handler for the root directory
:- http_handler(root(.), dashboard_page, []).

% Handler to serve the combined HTML, CSS, and JS
dashboard_page(_Request) :-
    reply_html_page(
        [title('Dashboard Example')],
        [\html_requires_resources]).

html_requires_resources -->
    html([
        \html_requires_head,
        body([
            \html_dashboard_body,
            \html_dashboard_scripts
        ])
    ]).

html_requires_head -->
    html(head([
        meta([charset='UTF-8']),
        meta([name='viewport', content='width=device-width, initial-scale=1.0']),
        script([src='https://cdn.jsdelivr.net/npm/chart.js'], []),
        script([src='https://code.jquery.com/jquery-3.7.1.min.js', integrity='sha256-/JqT3SQfawRcv/BIHPThkBvs0OEvtFFmqPF/lYI/Cxo=', crossorigin='anonymous'], []),
        \html_css
    ])).

html_css -->
    html(style('
        body { font-family: Arial, sans-serif; }
        .container { width: 80%; margin: 0 auto; }
        .container h1 { text-align: center; }
        .main-info { display: flex; justify-content: space-between; margin: 20px; }
        .main-info .chart-container { width: 75%; margin: 20px; }
        .info-card { width: 25%; margin: 50px 0 20px 20px; border: 1px solid #ddd; padding: 20px; 
                     border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                     background: linear-gradient(to bottom, #0681c4, #8410ffb1); color: white; }
        .info-card .info-card-content p { margin: 10px 0; }
        .rate-currency { display: flex; justify-content: space-between; margin: 20px; }
        .rate-currency .info-rate-currency { width: 50%; }'
    )).

html_dashboard_body -->
    html(div(class('container'), [
        h1('Final Project Prolog'),
        div(class('main-info'), [
            div(class('chart-container'), [
                h2('Currency Chart'),
                canvas([id('myLineChart'), width('400'), height('400')], [])
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
                canvas([id('lineRateCurrency'), width('400'), height('400')], [])
            ])
        ])
    ])).

html_dashboard_scripts -->
    html(script(type='text/javascript', [
        % Embed your JavaScript code here
        '
        $(document).ready(function() {
            getChartUsdtToIdr();
            getRateCurrency();
        });

        function getChartUsdtToIdr() {
            $.ajax({
                type: "get",
                url: "https://api.frankfurter.app/2020-01-27..?from=USD&to=IDR",
                dataType: "json",
                success: function (response) {
                    let chartLabels = Object.keys(response.rates);
                    let chartData = Object.values(response.rates).map(rate => rate.IDR);
                    let ctx = document.getElementById("myLineChart").getContext("2d");
                    let myLineChart = new Chart(ctx, {
                        type: "line",
                        data: {
                            labels: chartLabels,
                            datasets: [{
                                label: "IDR Exchange Rates",
                                data: chartData,
                                borderColor: "rgb(75, 192, 192)",
                                backgroundColor: "rgba(75, 192, 192, 0.1)",
                                borderWidth: 1,
                                fill: true
                            }]
                        },
                        options: {
                            responsive: true,
                            scales: {
                                y: {
                                    beginAtZero: false
                                }
                            }
                        }
                    });
                }
            });
        }

        function getRateCurrency() {
            $.ajax({
                type: "get",
                url: "https://open.er-api.com/v6/latest/USD",
                dataType: "json",
                success: function (response) {
                    let rateData = {
                        AUD: response.rates.AUD,
                        EUR: response.rates.EUR,
                        SGD: response.rates.SGD,
                        BMD: response.rates.BMD,
                        ANG: response.rates.ANG
                    };
                    let currencyLabels = Object.keys(rateData);
                    let currencyRates = Object.values(rateData);
                    let ctx = document.getElementById("lineRateCurrency").getContext("2d");
                    let rateCurrencyChart = new Chart(ctx, {
                        type: "doughnut",
                        data: {
                            labels: currencyLabels,
                            datasets: [{
                                label: "Currency Exchange Rates to USD",
                                data: currencyRates,
                                backgroundColor: [
                                    "rgb(255, 99, 132)",
                                    "rgb(54, 162, 235)",
                                    "rgb(255, 205, 86)"
                                ],
                                borderWidth: 2
                            }]
                        }
                    });
                }
            });
        }
        '
    ])).
