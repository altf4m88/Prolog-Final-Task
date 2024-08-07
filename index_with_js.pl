:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), dashboard_page, []).

dashboard_page(_Request) :-
    reply_html_page(
        title('Dashboard Example'),
        [\html_page_content]).

html_page_content -->
    html([
        \html_head,
        \html_body
    ]).

html_head -->
    html(head([
        meta([charset(utf8)]),
        meta([name(viewport), content('width=device-width, initial-scale=1.0')]),
        link([rel(stylesheet), href('https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css'), integrity('sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC'), crossorigin(anonymous)]),
        script([src('https://cdn.jsdelivr.net/npm/chart.js')], []),
        script([src('https://code.jquery.com/jquery-3.7.1.min.js')], []),
        \inline_styles
    ])).

html_body -->
    html(body([
        div(class(container), [
            h1(class('text-center my-5'), 'Final Project Prolog'),
            \form_currency,
            \info_section,
            \rate_currency_section,
            \inline_scripts
        ])
    ])).

form_currency -->
    html(div(class('card p-5 shadow-lg'), [
        h2('Form Currency'),
        form([
            \currency_selects,
            \amount_inputs,
            button([type(button), id(convertBtn), class('btn btn-primary')], 'Convert')
        ])
    ])).

currency_selects -->
    html([
        div(class('row mb-3'), [
            div(class('col-12 col-md-6 col-lg-6'), [
                label([for(fromCurrency)], 'From:'),
                select([class('form-select'), id(fromCurrency)], [])
            ]),
            div(class('col-12 col-md-6 col-lg-6'), [
                label([for(toCurrency)], 'To:'),
                select([class('form-select'), id(toCurrency)], [])
            ])
        ])
    ]).

amount_inputs -->
    html(div(class('mb-3'), [
        label([for(amount)], 'Amount:'),
        input([type(number), class('form-control'), id(amount), value('1')]),
        input([type(number), class('form-control mt-4'), id('amount-result'), disabled])
    ])).

info_section -->
    html(div(class('main-info card p-5 my-5 shadow-lg'), [
        div(class('row'), [
            div(class('col-12 col-md-9 col-lg-9'), [
                div(class('chart-container'), [
                    h2('Currency Chart'),
                    canvas(id('myLineChart'), [])
                ])
            ]),
            div(class('col-12 col-md-3 col-md-3'), [
                div(class('info-card'), [
                    div(class('info-card-content'), [
                        p([
                            strong('Currency:'), 
                            span(class('currency-from'), []), 
                            '/',
                            span(class('currency-to'), [])
                        ]),
                        p([
                            strong('Price:'), 
                            span(class('current-price'), '75'), 
                            span(class('currency-to'), [])
                        ]),
                        p([
                            strong('Date:'), 
                            span(class('latest-date-currency'), '2024-05-17')
                        ])
                    ])
                ])
            ])
        ])
    ])).

rate_currency_section -->
    html(div(class('card p-5 shadow-lg my-5'), [
        div(class('rate-currency text-center mx-auto'), [
            div(class('info-rate-currency'), [
                h2('Info Rate Currency By USD'),
                canvas(id('lineRateCurrency'), [])
            ])
        ])
    ])).

inline_styles -->
    html(style('
        body { font-family: Arial, sans-serif; }
        .info-card { margin: 20px; border: 1px solid #ddd; padding: 20px; margin: 50px 0;
                     border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                     background: linear-gradient(to bottom, #0681c4, #8410ffb1); color: white; }
        .info-card .info-card-content p { margin: 10px 0; }
    ')).

inline_scripts -->
    js_script({|javascript||
        $(document).ready(function() {
            $("#convertBtn").click(function (e) {
                let fromCurrency = $("#fromCurrency").val();
                let toCurrency = $("#toCurrency").val();
                let amount = $("#amount").val();

                getChart(fromCurrency, toCurrency, amount);
            });

            getChart("USD", "IDR");

            async function getChart(from = "USD", to = "IDR", amount = 1) {
                $(".currency-from").html(from);
                $(".currency-to").html(to);

                const data = await fetchCurrencyData(from, to, amount);

                let currentPrice = data.rates[data.end_date][to];

                $(".current-price").html(currentPrice);
                $("#amount-result").val(currentPrice);
                $(".latest-date-currency").html(data.end_date);

                createCurrencyChart(data, to);
            }

            async function fetchCurrencyData(fromCurrency, toCurrency, amount = 1) {
                const apiUrl = `https://api.frankfurter.app/2015-01-01..?from=${fromCurrency}&to=${toCurrency}&amount=${amount}`;
                try {
                    const response = await fetch(apiUrl);
                    const data = await response.json();
                    return data;
                } catch (error) {
                    console.error("Error fetching data:", error);
                    return null;
                }
            }

            let myLineChart; // Declare this outside the function to maintain its scope across multiple calls

            function createCurrencyChart(data, toCurrency) {
                if (!data) return;

                let chartLabels = Object.keys(data.rates);
                let chartData = Object.values(data.rates).map((rate) => rate[toCurrency]);

                let ctx = document.getElementById("myLineChart").getContext("2d");

                if (myLineChart) {
                    myLineChart.destroy();
                }

                myLineChart = new Chart(ctx, {
                    type: "line",
                    data: {
                        labels: chartLabels,
                        datasets: [
                            {
                                label: `${toCurrency}`,
                                data: chartData,
                                borderColor: "rgb(75, 192, 192)",
                                backgroundColor: "rgba(75, 192, 192, 0.1)",
                                borderWidth: 1,
                                fill: true,
                                pointStyle: false,
                            },
                        ],
                    },
                    options: {
                        responsive: true,
                        scales: {
                            y: {
                                beginAtZero: false,
                            },
                        },
                        plugins: {
                            legend: {
                                display: true,
                                labels: {
                                    color: "black",
                                },
                            },
                            tooltip: {
                                enabled: true,
                                mode: "index",
                                intersect: false,
                                callbacks: {
                                    label: function (tooltipItem) {
                                        return `${tooltipItem.dataset.label}: ${tooltipItem.formattedValue} ${toCurrency}`;
                                    },
                                },
                            },
                        },
                    },
                });
            }

            async function fetchCurrencyRate(baseCurrency) {
                const apiUrl = `https://open.er-api.com/v6/latest/${baseCurrency}`;
                try {
                    const response = await fetch(apiUrl);
                    const data = await response.json();
                    return data;
                } catch (error) {
                    console.error("Error fetching data:", error);
                    return null;
                }
            }

            function createRateCurrencyChart(data) {
                if (!data || !data.rates) return; // Ensure there is valid data

                let rateData = {
                    AUD: data.rates.AUD,
                    EUR: data.rates.EUR,
                    SGD: data.rates.SGD,
                    BMD: data.rates.BMD,
                    ANG: data.rates.ANG,
                    AED: data.rates.AED,
                };
                let currencyLabels = Object.keys(rateData);
                let currencyRates = Object.values(rateData);

                let ctx = document.getElementById("lineRateCurrency").getContext("2d");
                let rateCurrencyChart = new Chart(ctx, {
                    type: "doughnut",
                    data: {
                        labels: currencyLabels,
                        datasets: [
                            {
                                label: "Currency Exchange Rates to USD",
                                data: currencyRates,
                                backgroundColor: [
                                    "rgb(255, 99, 132)",
                                    "rgb(54, 162, 235)",
                                    "rgb(255, 205, 86)",
                                    "rgb(75, 192, 192)",
                                    "rgb(153, 102, 255)",
                                    "rgb(0, 0, 0)",
                                ],
                                borderWidth: 2,
                                fill: false,
                            },
                        ],
                    },
                    options: {
                        responsive: true,
                    },
                });
            }

            getRateCurrency();

            async function getRateCurrency(currency = "USD") {
                const data = await fetchCurrencyRate(currency);
                createRateCurrencyChart(data);
            }

            getCurrency();

            async function getCurrency() {
                const data = await fetchCurrencies("USD", "IDR");
                applyToForm(data);
            }

            function applyToForm(data) {
                if (!data) return;
                console.log(data);
                let htmlOption = "";

                $.each(data, function (index, currency) {
                    htmlOption += `<option value="${index}">${currency}</option>`;
                });

                $("#fromCurrency").html(htmlOption);
                $("#toCurrency").html(htmlOption);

                $("#fromCurrency").val("USD");
                $("#toCurrency").val("IDR");
            }

            async function fetchCurrencies() {
                const apiUrl = `https://api.frankfurter.app/currencies`;
                try {
                    const response = await fetch(apiUrl);
                    const data = await response.json();
                    return data;
                } catch (error) {
                    console.error("Error fetching data:", error);
                    return null;
                }
            }

        });
    |}).
