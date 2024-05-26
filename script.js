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
